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


(define (%forward-sexp #!optional (offset (point)) (buffer (current-buffer)))
  (let ((navigator (SchemeEditor:getExplorer buffer)))
    (if (SexpNavigator:forwardSexpression navigator offset)
        (values (SexpNavigator:getSexpStart navigator) (SexpNavigator:getSexpEnd navigator))
        (values #f #f))))


(define (%backward-sexp #!optional (offset (point)) (buffer (current-buffer)))
  (let ((navigator (SchemeEditor:getExplorer buffer)))
    (if (SexpNavigator:backwardSexpression navigator offset)
        (values (SexpNavigator:getSexpStart navigator) (SexpNavigator:getSexpEnd navigator))
        (values #f #f))))


(define (sexp-type #!optional (offset (point)) (buffer (current-buffer)))
  (let-values (((start end) (%forward-sexp offset buffer)))
    (and start end
         (let* ((navigator (SchemeEditor:getExplorer buffer))
                (type      (SexpNavigator:getSexpType navigator)))
           (if (<= type 5)
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

