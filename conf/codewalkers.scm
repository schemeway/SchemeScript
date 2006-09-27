;;;
;;;; Code walkers
;;;
;;
;; @created   "Wed Jul 12 15:26:16 EDT 2006"
;;


(define-namespace TextPresentation "class:org.eclipse.jface.text.TextPresentation")
(define-namespace StyleRange "class:org.eclipse.swt.custom.StyleRange")
(define-namespace ITextViewer "class:org.eclipse.jface.text.ITextViewer")
(define-namespace StyledText "class:org.eclipse.swt.custom.StyledText")

(define (annotate-defines editor stx-obj)
  (define *let-keywords* '(let let* letrec letrec*))
  
  (define (add-definition def-stx-obj)
    (let ((tp (TextPresentation:new))
          (sr (StyleRange:new)))
      (set! (StyleRange:.start sr) (stx-object-offset def-stx-obj))
      (set! (StyleRange:.length sr) (stx-object-length def-stx-obj))
      (set! (StyleRange:.underline sr) #t)
      (set! (StyleRange:.foreground sr) (Color:new #!null 0 0 128))
      (TextPresentation:addStyleRange tp sr)
      (ITextViewer:changeTextPresentation (SchemeEditor:getTextViewer editor) tp #t)))
  
  (define (add-constant cst-stx-obj)
    (let ((tp (TextPresentation:new))
          (sr (StyleRange:new)))
      (set! (StyleRange:.start sr) (stx-object-offset cst-stx-obj))
      (set! (StyleRange:.length sr) (stx-object-length cst-stx-obj))
      (set! (StyleRange:.fontStyle sr) (SWT:.ITALIC))
      (set! (StyleRange:.foreground sr) (Color:new #!null 0 128 0))
      (TextPresentation:addStyleRange tp sr)
      (ITextViewer:changeTextPresentation (SchemeEditor:getTextViewer editor) tp #t)))
  
  (define (add-variables vars)
    (let loop ((vars vars))
      (cond ((pair? vars)
             (let ((var (car vars)))
               (cond ((stx-symbol? var)
                      (add-variable var))
                     ((and (stx-list? var)
                           (= 2 (length (stx-object-data var)))
                           (stx-symbol? (car (stx-object-data var))))
                      (add-variable (car (stx-object-data var)))))
               (loop (cdr vars))))
            ((stx-symbol? vars)
             (add-variable vars)))))
  
  (define (add-variable var-stx-obj)
    (let ((tp (TextPresentation:new))
          (sr (StyleRange:new)))
      (set! (StyleRange:.start sr) (stx-object-offset var-stx-obj))
      (set! (StyleRange:.length sr) (stx-object-length var-stx-obj))
      (set! (StyleRange:.foreground sr) (Color:new #!null 0 0 192))
      (set! (StyleRange:.fontStyle sr) (SWT:.ITALIC))
      (TextPresentation:addStyleRange tp sr)
      (ITextViewer:changeTextPresentation (SchemeEditor:getTextViewer editor) tp #t)))
  
  (define (process-binding binding-obj)
    (and (stx-list? binding-obj)
         (let ((elements (stx-object-data binding-obj)))
           (and (list? elements)
                (= (length elements) 2)
                (stx-symbol? (car elements))
                (add-variable (car elements))))))
  
  (define (scan-objects stx-objs)
    (let loop ((stx-objs stx-objs))
      (cond ((pair? stx-objs)
             (scan-object (car stx-objs))
             (loop (cdr stx-objs)))
            (else
             (when (not (null? stx-objs))
               (scan-object stx-objs))))))

  
  (define (scan-object stx-obj) 
    (cond ((and stx-obj
                (stx-list? stx-obj)
                (let ((elts (stx-object-data stx-obj)))
                  (and (list? elts)
                       (>= (length elts) 3)
                       (stx-symbol? (car elts)) 
                       (equal? (stx-object-data (car elts)) 'define)
                       (pair? (cdr elts))
                       (stx-list? (cadr elts))
                       (pair? (stx-object-data (cadr elts)))
                       (stx-symbol? (car (stx-object-data (cadr elts))))
                       (car (stx-object-data (cadr elts))))))
           =>
           (lambda (var-stx-obj)
             (add-definition var-stx-obj)
             (add-variables (cdr (stx-object-data (cadr (stx-object-data stx-obj)))))
             (for-each scan-object (cddr (stx-object-data stx-obj)))))

          ((and stx-obj
                (stx-list? stx-obj)
                (let ((elts (stx-object-data stx-obj)))
                  (and (list? elts)
                       (= (length elts) 3)
                       (stx-symbol? (car elts))
                       (equal? (stx-object-data (car elts)) 'define)
                       (pair? (cdr elts))
                       (eq? (stx-object-type (cadr elts))  'symbol)
                       (cadr elts))))
           =>
           (lambda (var-stx-obj)
             (add-definition var-stx-obj)
             (for-each scan-object (cdr (stx-object-data stx-obj)))))
          
          ((and stx-obj
                (stx-list? stx-obj)
                (let ((elts (stx-object-data stx-obj)))
                  (and (pair? elts)
                       (stx-symbol? (car elts))
                       (equal? (stx-object-data (car elts)) 'define))))
           (let ((offset (stx-object-offset stx-obj)))
             (add-marker/offset! 'error
                                 (buffer-file editor)
                                 (+ 1 (offset-line offset editor))
                                 offset
                                 (+ offset (stx-object-length stx-obj))
                                 "Ill-formed 'define'")))

          ((and stx-obj
                (eq? (stx-object-type stx-obj) 'list)
                (let ((elts (stx-object-data stx-obj)))
                  (and (pair? elts)
                       (eq? (stx-object-type (car elts)) 'symbol)
                       (equal? (stx-object-data (car elts)) 'lambda)
                       (pair? (cdr elts))
                       (eq? (stx-object-type (cadr elts))  'list))))
           (add-variables (stx-object-data (cadr (stx-object-data stx-obj))))
           (for-each scan-object (cddr (stx-object-data stx-obj))))
          
          ((and stx-obj
                (eq? (stx-object-type stx-obj) 'list)
                (let ((elts (stx-object-data stx-obj)))
                  (and (pair? elts)
                       (eq? (stx-object-type (car elts)) 'symbol)
                       (member (stx-object-data (car elts)) *let-keywords*)
                       (pair? (cdr elts))
                       (eq? (stx-object-type (cadr elts))  'list))))
           
           (for-each process-binding (stx-object-data (cadr (stx-object-data stx-obj))))
           (for-each scan-object (cddr (stx-object-data stx-obj))))
          
          ((and stx-obj
                (stx-list? stx-obj)
                (>= (length (stx-object-data stx-obj)) 3)
                (let ((elts (stx-object-data stx-obj)))
                  (and (pair? elts)
                       (stx-symbol? (car elts))
                       (member (stx-object-data (car elts)) *let-keywords*)
                       (pair? (cdr elts))
                       (stx-symbol? (cadr elts))
                       (pair? (cddr elts))
                       (stx-list? (caddr elts)))))
           
           (add-variable (cadr (stx-object-data stx-obj)))
           (for-each process-binding (stx-object-data (caddr (stx-object-data stx-obj))))
           (for-each scan-object (cddr (stx-object-data stx-obj))))
          
          ((and stx-obj
                (eq? (stx-object-type stx-obj) 'list))
           (scan-objects (stx-object-data stx-obj)))))
  
  (scan-object stx-obj))

(define (check-buffer-syntax editor)
  (clear-markers (SchemeEditor:getFile editor))
  (catch 'syntax-error
    (lambda ()
      (stx-read-all (SchemeEditor:getDocument editor) 
                    (lambda (stx-obj) (annotate-defines editor stx-obj))))
    (lambda (key message offset len)
      (add-marker/offset! 'error (SchemeEditor:getFile editor) (+ 1 (offset-line offset)) offset (+ offset len) message)
      '())))


(add-save-hook 'check-buffer-syntax)
