;;;
;;;; Buffer-related functions
;;;
;;
;; @created   "Tue Feb 22 09:35:28 EST 2005"
;; @author    "Dominique Boucher"
;; @copyright "NuEcho Inc."
;;


(define-namespace SchemeEditor     "class:org.schemeway.plugins.schemescript.editor.SchemeEditor")
(define-namespace SexpNavigator    "class:org.schemeway.plugins.schemescript.parser.SexpNavigator")
(define-namespace SymbolDictionary "class:org.schemeway.plugins.schemescript.dictionary.ISymbolDictionary")
(define-namespace ScmTextUtil      "class:org.schemeway.plugins.schemescript.editor.SchemeTextUtilities")
(define-namespace Region           "class:org.eclipse.jface.text.Region")
(define-namespace Document         "class:org.eclipse.jface.text.IDocument")
(define-namespace MessageDialog    "class:org.eclipse.jface.dialogs.MessageDialog")


(define (current-buffer)
  (let ((page (org.eclipse.ui.IWorkbenchWindow:getActivePage
               (org.eclipse.ui.IWorkbench:getActiveWorkbenchWindow
                (org.eclipse.ui.PlatformUI:getWorkbench)))))
    (org.eclipse.ui.IWorkbenchPage:getActiveEditor page)))


(define (point #!optional (buffer (current-buffer)))
  (SchemeEditor:getPoint buffer))


(define (set-point point #!optional (buffer (current-buffer)))
  (SchemeEditor:setPoint buffer point))


(define (set-selection point-min point-max #!optional (buffer (current-buffer)))
  (SchemeEditor:setSelection buffer point-min point-max))


(define (with-selection proc #!optional (buffer (current-buffer)))
  (let* ((selection (SchemeEditor:getSelection buffer))
         (start     (Region:getOffset selection))
         (end       (+ start (Region:getLength selection))))
    (proc start end)))



(define (forward-sexp #!optional (offset (point)) (buffer (current-buffer)))
  (let ((navigator (SchemeEditor:getExplorer buffer)))
    (if (SexpNavigator:forwardSexpression navigator offset)
        (values (SexpNavigator:getSexpStart navigator) (SexpNavigator:getSexpEnd navigator))
        (values #f #f))))


(define (backward-sexp #!optional (offset (point)) (buffer (current-buffer)))
  (let ((navigator (SchemeEditor:getExplorer buffer)))
    (if (SexpNavigator:backwardSexpression navigator offset)
        (values (SexpNavigator:getSexpStart navigator) (SexpNavigator:getSexpEnd navigator))
        (values #f #f))))


(define (buffer-size #!optional (buffer (current-buffer)))
  (Document:getLength (SchemeEditor:getDocument buffer)))


(define (with-buffer-text start end proc #!optional (buffer (current-buffer)))
  (when (< start 0)
    (error "start offset must be greater or equal to 0."))
  (when (> end (buffer-size buffer))
    (error "end offset must not be greater than the buffer size"))
  (let ((text (make <string> (SchemeEditor:getText buffer start (- end start)))))
    (proc text)))


(define (buffer-text start end #!optional (buffer (current-buffer)))
  (with-buffer-text start end (lambda (text) text) buffer))

(define (looking-at text #!optional (offset (point)) (buffer (current-buffer)))
  (with-buffer-text offset (+ offset (string-length text)) 
                    (lambda (str) (string=? text str))
                    buffer))


(define (insert-text text #!optional (offset (point)) (buffer (current-buffer)))
  (SchemeEditor:insertText buffer offset (as <String> text)))


(define (delete-text len #!optional (offset (point)) (buffer (current-buffer)))
  (SchemeEditor:replaceText buffer offset len (as <String> "")))


(define (symbol-near-point #!optional (offset (point)) (buffer (current-buffer)))
  (let* ((document (SchemeEditor:getDocument buffer))
         (text     (ScmTextUtil:findSymbolAroundPoint document offset)))
    (if (eq? text #!null)
        #f
        (string->symbol (symbol->string text)))))


(define (line-offset line #!optional (buffer (current-buffer)))
  (Document:getLineOffset (SchemeEditor:getDocument buffer) line))


(define (offset-line offset #!optional (buffer (current-buffer)))
  (Document:getLineOfOffset (SchemeEditor:getDocument buffer) offset))


(define (sexp-type #!optional (buffer (current-buffer)))
  (and (forward-sexp buffer)
       (let* ((navigator (SchemeEditor:getExplorer buffer))
              (type      (SexpNavigator:getSexpType navigator)))
         (if (<= type 5)
             (vector-ref '#(error: symbol: list: string: constant: other:) type)
             none:))))


(define (message-box title message)
  (MessageDialog:openInformation #!null title message))

