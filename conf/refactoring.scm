;;;
;;;; Refactoring tools for Scheme
;;;
;;
;; @created   "Tue Jun 06 17:00:30 EDT 2006"
;; @author    "Dominique Boucher"
;; @copyright "(c) 2006 Nu Echo Inc. All rights reserved."
;;


(require 'srfi-1)


(define-namespace SchemeScriptPlugin "class:org.schemeway.plugins.schemescript.SchemeScriptPlugin")
(define-namespace SymbolReferencesManager "class:org.schemeway.plugins.schemescript.dictionary.SymbolReferencesManager")
(define-namespace SymbolReferencesTable "class:org.schemeway.plugins.schemescript.dictionary.SymbolReferencesTable")
(define-namespace Reference "class:org.schemeway.plugins.schemescript.dictionary.Reference")

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


;;;
;;;; Linked model (with or without UI)
;;;



(define scan-references
  (let ((references-manager (SchemeScriptPlugin:getReferencesManager)))
    (lambda (buffer)
      (SymbolReferencesManager:scanResourceForSymbols references-manager (buffer-file buffer)))))


(define (make-model-linking-listener thunk)
  (object (<org.eclipse.jface.text.link.ILinkedModeListener> <Object>)
    ((left (model :: <org.eclipse.jface.text.link.LinkedModeModel>) (flags :: <int>)) :: <void>
     (thunk))
    ((suspend (model :: <org.eclipse.jface.text.link.LinkedModeModel>)) :: <void>
     #!void)
    ((resume (model :: <org.eclipse.jface.text.link.LinkedModeModel>) (flags :: <int>)) :: <void>
     #!void)))


(define (create-linked-mode/ui positions #!optional (buffer (current-buffer)))
  (let* ((model    (LinkedModeModel:new))
         (group    (LinkedPositionGroup:new)))
    (for-each (lambda (pos)
                (let ((position (LinkedPosition:new (car pos) (cadr pos) (caddr pos) 0)))
                  (LinkedPositionGroup:addPosition group position)))
              positions)

    (LinkedModeModel:addGroup model group)
    (if (LinkedModeModel:tryInstall model)
        (let* ((viewer (SchemeEditor:getTextViewer buffer)))
          (let ((ui (LinkedModeUI:new (as <org.eclipse.jface.text.link.LinkedModeModel> model)
                                      (as <org.eclipse.jface.text.ITextViewer> viewer))))
            (LinkedModeUI:setCyclingMode ui (LinkedModeUI:.CYCLE_ALWAYS))
            (LinkedModeUI:enter ui)))
        (message-box "Linked mode UI" "Unable to create linked mode model."))))


(define (create-linked-mode positions #!optional (model-listener #f) (buffer (current-buffer)))
  (let* ((model    (LinkedModeModel:new))
         (group    (LinkedPositionGroup:new)))
    (for-each (lambda (pos)
                (let ((position (LinkedPosition:new (car pos) (cadr pos) (caddr pos) 0)))
                  (LinkedPositionGroup:addPosition group position)))
              positions)

    (LinkedModeModel:addGroup model group)
    (when model-listener
      (LinkedModeModel:addLinkingListener model model-listener))
    (if (LinkedModeModel:tryInstall model)
        (lambda ()
          (LinkedModeModel:exit model 1))
        (begin
          (message-box "Linked mode UI" "Unable to create linked mode model.")
          #f))))


;;;
;;;; Renaming symbols
;;;


(define (%rename-symbol offset len name #!optional (resource #f))

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
               (let ((buffer (ITextFileBufferManager:getTextFileBuffer buffer-manager (as <org.eclipse.core.runtime.IPath> path))))
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


  (define (rename-locally positions)
    (create-linked-mode/ui positions))

  (define (rename-globally positions buffer)
    (let ((close-model (create-linked-mode positions (make-model-linking-listener close-non-shared-documents))))
      (if close-model
          (let ((new-name (ask-for-symbol "Rename Symbol Globally" "New name:" name)))
            (when new-name
              (let ((document (SchemeEditor:getDocument buffer)))
                (IDocument:replace document offset len new-name)))
            (close-model))
          (message-box "Renaming problem" "Unable to create linked mode model."))))


  (define (get-references)
    (array->list
     (if resource
         (SymbolReferencesTable:getReferences references-table name resource)
         (SymbolReferencesTable:getReferences references-table name))))

  ;; this function has side-effects... the variable 'all-resources' will 
  ;; contain the set of all resources affected by the refactoring...
  (define (gather-positions)
    (map (lambda (ref)
           (list (ref->document ref) (Reference:.offset ref) (Reference:.length ref)))
         (get-references)))

  
  (let ((buffer (current-buffer)))
    (scan-references buffer)
    (let ((positions (gather-positions)))
      (if (= 1 (length all-resources))
          (rename-locally positions)
          (try-finally
              (rename-globally positions buffer)
            (close-non-shared-documents))))))


(define (rename-symbol)
  (let-values (((start end) (%backward-sexp)))
    (when (and start end (eq? (sexp-type start) symbol:))
      (with-buffer-text
       start end
       (lambda (text)
         (%rename-symbol start (- end start) text))))))


(define (rename-symbol-locally)
  (let-values (((start end) (%backward-sexp)))
    (when (and start end (eq? (sexp-type start) symbol:))
      (with-buffer-text
       start end
       (lambda (text)
         (%rename-symbol start (- end start) text (buffer-file (current-buffer))))))))


;;;
;;;; Creating a new function
;;;


(define (process-next-expression-at-top proc)
  (let-values (((start end) (%forward-sexp)))
    (when (and start end)
      (let-values (((start end) (%backward-sexp end)))
        (with-buffer-text start end
          (lambda (text)
            (set-point start)
            (with-top-sexp
             (lambda (next-start next-end)
               (set-point next-end)
               (proc start end text next-start next-end)))))))))


(define (create-function-from-expression)
  (process-next-expression-at-top 
   (lambda (start end text next-start next-end)
     (let* ((expr (format #f "~%~%~%(define ~a~%  )~%" text))
            (len  (string-length expr)))
       (insert-text expr)
       (forward-char (- len 3))))))


;;;
;;;; Extracting a new top-level variable
;;;


(define (extract-variable)
  (process-next-expression-at-top
   (lambda (start end text next-start next-end)
     (let* ((expr            (format #f "~%~%~%(define var~%  ~a)~%" text))
            (expr-len        (string-length expr))
            (insertion-point (+ (- next-end expr-len) 3)))
       (delete-text (- end start) start)
       (insert-text "var" start)
       (insert-text expr)
       (indent-region insertion-point (+ insertion-point expr-len))
       (let ((document (buffer-document (current-buffer))))
         (create-linked-mode/ui (list (list document start 3)
                                      (list document (+ (point) 14) 3)))))))) ;; TODO - check that 14 is valid under Unix...



