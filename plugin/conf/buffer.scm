;;;
;;;; Buffer-related functions
;;;
;;
;; @created   "Tue Feb 22 09:35:28 EST 2005"
;; @author    "Dominique Boucher"
;; @copyright "NuEcho Inc."
;;


(define (current-buffer)
  (let ((page (org.eclipse.ui.IWorkbenchWindow:getActivePage
               (org.eclipse.ui.IWorkbench:getActiveWorkbenchWindow
                (org.eclipse.ui.PlatformUI:getWorkbench)))))
    (org.eclipse.ui.IWorkbenchPage:getActiveEditor page)))

(define (buffer-document #!optional (buffer (current-buffer)))
  (SchemeEditor:getDocument buffer))


(define (point #!optional (buffer (current-buffer)))
  (SchemeEditor:getPoint buffer))


(define (set-point point #!optional (buffer (current-buffer)))
  (SchemeEditor:setPoint buffer point))


(define (char-at point #!optional (buffer (current-buffer)))
  (SchemeEditor:getChar buffer point))


(define (point-max #!optional (buffer (current-buffer)))
  (IDocument:getLength (SchemeEditor:getDocument buffer)))


(define (set-selection point-min point-max #!optional (buffer (current-buffer)))
  (SchemeEditor:setSelection buffer point-min point-max))


(define (with-selection proc #!optional (buffer (current-buffer)))
  (let* ((selection (SchemeEditor:getSelection buffer))
         (start     (Region:getOffset selection))
         (end       (+ start (Region:getLength selection))))
    (proc start end)))


(define (%forward-sexp #!optional (offset (point)) (buffer (current-buffer)))
  (let ((navigator (SchemeEditor:getExplorer buffer)))
    (if (SexpNavigator:forwardSexpression navigator offset)
        (values (SexpNavigator:getSexpStart navigator) (SexpNavigator:getSexpEnd navigator))
        (values #f #f))))


(define (with-forward-sexp offset buffer proc)
  (let-values (((start end) (%forward-sexp offset buffer)))
    (if (and start end)
        (proc start end))))


(define (%backward-sexp #!optional (offset (point)) (buffer (current-buffer)))
  (let ((navigator (SchemeEditor:getExplorer buffer)))
    (if (SexpNavigator:backwardSexpression navigator offset)
        (values (SexpNavigator:getSexpStart navigator) (SexpNavigator:getSexpEnd navigator))
        (values #f #f))))


(define (with-backward-sexp offset buffer proc)
  (let-values (((start end) (%backward-sexp offset buffer)))
    (if (and start end)
        (proc start end))))


(define (%up-sexp #!optional (offset (point)) (buffer (current-buffer)))
  (let ((navigator (SchemeEditor:getExplorer buffer)))
    (and (SexpNavigator:upSexpression navigator offset)
         (SexpNavigator:getSexpStart navigator))))


(define (with-up-sexp offset buffer proc)
  (let ((start (%up-sexp offset buffer)))
    (if start
        (with-forward-sexp start buffer proc))))


(define (with-top-sexp proc #!optional (offset (point)) (buffer (current-buffer)))
  (let loop ((top-offset offset))
    (let ((up-offset (%up-sexp top-offset buffer)))
      (if up-offset
          (loop up-offset)
          (let-values (((top-start top-end) (%forward-sexp top-offset buffer)))
            (if (and top-start top-end)
                (proc top-start top-end)
                #f))))))




(define (sexp-type #!optional (offset (point)) (buffer (current-buffer)))
  (let-values (((start end) (%forward-sexp offset buffer)))
    (and start end
         (let* ((navigator (SchemeEditor:getExplorer buffer))
                (type      (SexpNavigator:getSexpType navigator)))
           (if (and (integer? type) (<= type 5))
               (vector-ref '#(error: symbol: list: string: constant: other:) type)
               none:)))))


(define (buffer-size #!optional (buffer (current-buffer)))
  (IDocument:getLength (SchemeEditor:getDocument buffer)))


(define (with-buffer-text start end proc #!optional (buffer (current-buffer)))
  (when (< start 0)
    (error "start offset must be greater or equal to 0."))
  (when (> end (buffer-size buffer))
    (error "end offset must not be greater than the buffer size"))
  (let ((text (make <string> (SchemeEditor:getText buffer start (- end start)))))
    (proc text)))


(define (run-compound-change proc #!optional (buffer (current-buffer)))
  (try-finally 
      (begin
        (SchemeEditor:startCompoundChange buffer)
        (proc))
    (begin
      (SchemeEditor:endCompoundChange buffer)
      #f)))


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
  (IDocument:getLineOffset (SchemeEditor:getDocument buffer) line))


(define (offset-line offset #!optional (buffer (current-buffer)))
  (IDocument:getLineOfOffset (SchemeEditor:getDocument buffer) offset))


(define (beginning-of-line #!optional (offset (point)) (buffer (current-buffer)))
  (set-point (line-offset (offset-line offset buffer) buffer) buffer))


(define (forward-char #!optional (n 1) (buffer (current-buffer)))
  (set-point (+ (point) n)))

(define (backward-char #!optional (n 1) (buffer (current-buffer)))
  (set-point (- (point) n)))


(define (forward-line #!optional (n 1) (buffer (current-buffer)))
  (when (not (= n 0))
    (let* ((current-line (offset-line (point) buffer))
           (new-line     (+ current-line n))
           (new-point    (line-offset new-line buffer)))
      (set-point new-point buffer))))


(define (backward-line #!optional (n 1) (buffer (current-buffer)))
  (forward-line (- n) buffer))


(define (forward-sexp #!optional (offset (point)) (buffer (current-buffer)))
  (let-values (((_ end) (%forward-sexp offset buffer)))
    (when end
      (set-point end buffer))))


(define (backward-sexp #!optional (offset (point)) (buffer (current-buffer)))
  (let-values (((start _) (%backward-sexp offset buffer)))
    (when start
      (set-point start buffer))))


;; indentation
(define (indent-region start end #!optional (buffer (current-buffer)))
  (let ((start-line (offset-line start buffer))
        (end-line   (offset-line end buffer))
        (explorer   (SchemeEditor:getExplorer buffer))
        (manager    (SchemeEditor:getIndentationManager buffer)))
    (run-compound-change
     (lambda ()
       (FormatAction:indentLines (buffer-document buffer) start-line end-line
                                 (SchemeIndentationContext:new explorer manager 0)
                                 buffer
                                 start))
     buffer)))



;; Adds a function that is called when a Scheme buffer is saved.
;; The function must accept one parameter, the buffer saved.
(define (add-save-hook hook)
  (if (symbol? hook)
      (SchemeEditor:addSaveHook hook)
      (error "invalid save hook: must be a symbol")))

(define (remove-save-hook hook)
  (if (symbol? hook)
      (SchemeEditor:removeSaveHook hook)
      (error "invalid save hook: must be a symbol")))

;; Returns the filename of the buffer
(define (buffer-file-name #!optional (buffer (current-buffer)))
  (SchemeEditor:getFileName buffer))


;; Returns the file corresponding to the buffer
(define (buffer-file #!optional (buffer (current-buffer)))
  (SchemeEditor:getFile buffer))


(define (with-document-from-file (ifile :: <org.eclipse.core.resources.IFile>) proc)
  (let ((buffer-manager (FileBuffers:getTextFileBufferManager))
        (path           (*:getFullPath ifile)))
    (try-finally
        (begin
          (ITextFileBufferManager:connect buffer-manager path #!null)
          (proc (*:getDocument (ITextFileBufferManager:getTextFileBuffer buffer-manager path))))
      (ITextFileBufferManager:disconnect buffer-manager path #!null))))

