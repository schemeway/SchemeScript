;;;
;;;; Refactoring tools for Scheme
;;;
;;
;; @created   "Tue Jun 06 17:00:30 EDT 2006"
;; @author    "Dominique Boucher"
;; @copyright "(c) 2006 Nu Echo Inc. All rights reserved."
;;


(define-namespace SchemeScriptPlugin "class:org.schemeway.plugins.schemescript.SchemeScriptPlugin")
(define-namespace SymbolReferencesManager "class:org.schemeway.plugins.schemescript.dictionary.SymbolReferencesManager")
(define-namespace SymbolReferencesTable "class:org.schemeway.plugins.schemescript.dictionary.SymbolReferencesTable")
(define-namespace Reference "class:org.schemeway.plugins.schemescript.dictionary.Reference")
(define-namespace ModelWithExitHook "class:org.schemeway.plugins.schemescript.dictionary.LinkedModeModelWithExitHook")

(define-namespace LinkedModeModel "class:org.eclipse.jface.text.link.LinkedModeModel")
(define-namespace LinkedPositionGroup "class:org.eclipse.jface.text.link.LinkedPositionGroup")
(define-namespace LinkedPosition "class:org.eclipse.jface.text.link.LinkedPosition")
(define-namespace LinkedModeUI "class:org.eclipse.jface.text.link.LinkedModeUI")
(define-namespace LinkedModeManager "class:org.eclipse.jface.text.link.LinkedModeManager")


(define-namespace FileBuffers "class:org.eclipse.core.filebuffers.FileBuffers")
(define-namespace IFileBuffer "class:org.eclipse.core.filebuffers.IFileBuffer")
(define-namespace IFileBufferManager "class:org.eclipse.core.filebuffers.IFileBufferManager")
(define-namespace ITextFileBuffer "class:org.eclipse.core.filebuffers.ITextFileBuffer")
(define-namespace ITextFileBufferManager "class:org.eclipse.core.filebuffers.ITextFileBufferManager")

(require 'srfi-1)


(define scan-references
  (let ((references-manager (SchemeScriptPlugin:getReferencesManager)))
    (lambda (buffer)
      (SymbolReferencesManager:scanResourceForSymbols references-manager (buffer-file buffer)))))


(add-save-hook 'scan-references)


;;;
;;;; Renaming symbols
;;;


(define (rename-scheme-symbol offset len name #!optional (resource #f))
  
  (define buffer-manager     (FileBuffers:getTextFileBufferManager))
  (define references-manager (SchemeScriptPlugin:getReferencesManager))
  (define references-table   (SymbolReferencesManager:getSymbolReferencesTable references-manager))
  
  (define all-resources '())
  
  
  (define (ref->document reference)
    (let ((resource (Reference:.resource reference)))
      (cond ((assoc resource all-resources)
             => (lambda (resource/buffer)
                  (let ((buffer (cdr resource/buffer)))
                    (ITextFileBuffer:getDocument buffer))))
            (else
             (let ((path (IResource:getFullPath resource)))
               (ITextFileBufferManager:connect buffer-manager path #!null)
               (let ((buffer (ITextFileBufferManager:getTextFileBuffer buffer-manager path)))
                 (set! all-resources (cons (cons resource buffer) all-resources))
                 (ITextFileBuffer:getDocument buffer)))))))
  
  
  (define (close-non-shared-documents)
    (for-each (lambda (resource/buffer)
                (let ((buffer (cdr resource/buffer))
                      (resource (car resource/buffer)))
                  (IFileBuffer:commit buffer #!null #t)
                  (IFileBufferManager:disconnect buffer-manager (IResource:getFullPath resource) #!null)
                  (LinkedModeModel:closeAllModels (ITextFileBuffer:getDocument buffer))
                  (SymbolReferencesManager:scanResourceForSymbols references-manager resource)))
              all-resources)
    (set! all-resources '()))
  
  
  (define (all-documents-open)
    (every (lambda (resource/buffer)
             (IFileBuffer:isShared (cdr resource/buffer)))
           all-resources))
  
  
  (define (ui-rename model buffer)
    (if (LinkedModeModel:tryInstall model)
        (let ((ui (LinkedModeUI:new model (SchemeEditor:getTextViewer buffer))))
          (LinkedModeUI:enter ui))
        (message-box "Renaming problem" "Unable to create linked mode model.")))
  
  
  (define (global-rename model buffer)
    (if (LinkedModeModel:tryInstall model)
        (let ((new-name (ask-for-symbol "Rename Symbol Globally" "New name:" name)))
          (when new-name
            (let ((document (SchemeEditor:getDocument buffer)))
              (IDocument:replace document offset len new-name)))
          (LinkedModeModel:exit model 1))
        (message-box "Renaming problem" "Unable to create linked mode model.")))

  
  (define (get-references)
    (if resource
        (array->list (SymbolReferencesTable:getReferences references-table name resource))
        (array->list (SymbolReferencesTable:getReferences references-table name))))
  
  
  (define (create-and-populate-linked-mode-model buffer)
    (let* ((model (ModelWithExitHook:new close-non-shared-documents))
           (group (LinkedPositionGroup:new)))
      (for-each (lambda (ref)
                  (let ((position (LinkedPosition:new (ref->document ref) (Reference:.offset ref) (Reference:.length ref) 0)))
                    (LinkedPositionGroup:addPosition group position)))
                (get-references))
      (LinkedModeModel:addGroup model group)
      model))
  

  (let ((buffer (current-buffer)))
    (scan-references buffer)
    (let ((model (create-and-populate-linked-mode-model buffer)))
      (if (= 1 (length all-resources))
          (ui-rename model buffer)
          (try-finally
              (global-rename model buffer)
            (close-non-shared-documents))))))


(define (interactive-rename-symbol)
  (let-values (((start end) (%backward-sexp)))
    (when (and start end (eq? (sexp-type start) symbol:))
      (with-buffer-text
       start end
       (lambda (text)
         (rename-scheme-symbol start (- end start) text))))))


(define (interactive-rename-symbol-locally)
  (let-values (((start end) (%backward-sexp)))
    (when (and start end (eq? (sexp-type start) symbol:))
      (with-buffer-text
       start end
       (lambda (text)
         (rename-scheme-symbol start (- end start) text (buffer-file (current-buffer))))))))

