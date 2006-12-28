;;;
;;;; Simple Scheme reader
;;;
;;
;; @created   "Fri Jun 09 09:03:59 EDT 2006"
;; @copyright "NuEcho Inc."
;;


(define-record-type <StxObject>
  (make-syntax-object type offset length data)
  stx-object?
  (type   stx-object-type)    ; type is one of '(list vector symbol constant special)
  (offset stx-object-offset)  ; offset is an integer
  (length stx-object-length)  ; length is an integer
  (data   stx-object-data))   ; data is some type-specific information


(define (stx-object->list stx-obj) 
  (unless (stx-object? stx-obj)
    (error "invalid argument to stx-object->list: " stx-obj))
  (unless (eq? 'type (stx-object-type stx-obj))
    (error "type of stx-obj must be 'list"))
  (stx-object-data stx-object))

(define (stx-symbol? stx-obj)
  (and (stx-object? stx-obj)
       (eq? 'symbol (stx-object-type stx-obj))))

(define (stx-list? stx-obj)
  (and (stx-object? stx-obj)
       (eq? 'list (stx-object-type stx-obj))))

(define (stx-special? stx-obj)
  (and (stx-object? stx-obj)
       (eq? 'special (stx-object-type stx-obj))))


(define (stx-read document start-offset error-handler)
  (define scanner (SchemeScanner:new))
  
  (define (init)
    (SchemeScanner:setRange scanner document start-offset (IDocument:getLength document)))
  
  (define token-offset #f)
  (define token-length #f)
  (define current-token #f)

  (define (consume)
    (set! current-token #f))
  
  (define (install-token tok)
    (set! current-token (SchemeToken:getType tok))
    (set! token-offset (SchemeToken:getOffset tok))
    (set! token-length (SchemeToken:getLength tok))
    current-token)
 
  (define (get-next-token)
    (or current-token
        (let loop ()
          (let* ((token (SchemeScanner:nextToken scanner))
                 (type  (SchemeToken:getType token)))
            (cond ((equal? type (SchemeToken:.EOFTOK))                   
                   (install-token token))
                  ((equal? type (SchemeToken:.COMMENT))
                   (loop))
                  ((equal? type (SchemeToken:.EXPR_COMMENT_PREFIX))
                   (read1)
                   (loop))
                  ((equal? type (SchemeToken:.WSPACE))
                   (loop))
                  (else
                   (install-token token)))))))
 
  (define (read-quote/unquote/quasiquote type)
    (let* ((offset  token-offset)
           (len     token-length)
           (element (begin (consume) (read1))))
      (make-syntax-object 'list offset (- (+ (stx-object-offset element) (stx-object-length element)) offset)
                          (list (make-syntax-object 'symbol offset len type)
                                element))))

  
  (define (read-list)
    (let ((offset token-offset)
          (len    token-length))
      (consume)
      (let* ((elements (read* #f))
             (token    (get-next-token)))
        (if (equal? token (SchemeToken:.RPAREN))
            (let ((len (+ (- token-offset offset) token-length)))
              (consume)
              (make-syntax-object 'list offset len elements))
            (error-handler "Missing closing parenthesis" offset len)))))
  

  (define (read-vector)
    (let ((offset token-offset)
          (len    2))
      ;; VECTORPREFIX retourne seulement le '#', a condition qu'il soit suivi du '('...
      ;; il faut donc le consommer explicitement...
      (consume) (get-next-token) (consume)
      (let* ((elements (read* #t))
             (token    (get-next-token)))
        (if (equal? token (SchemeToken:.RPAREN))
            (let ((len (+ (- token-offset offset) token-length)))
              (consume)
              (make-syntax-object 'vector offset len (list->vector elements)))
            (error-handler "Missing closing parenthesis" offset len)))))
  
  
  (define (read-string)
    (let ((offset token-offset)
          (len    token-length))
      (consume)
      (make-syntax-object 'string offset len 
                          (symbol->string (SchemeScanner:getText scanner offset len)))))
  
  
  (define (read-constant)
    (let* ((offset token-offset)
           (len    token-length)
           (text   (symbol->string (SchemeScanner:getText scanner offset len)))
           (val    (call-with-input-string text read)))
      (consume)
      (make-syntax-object 'constant offset len val)))
  
  
  (define (read* proper?)
    (let loop ((elements '()))
      (let ((token (get-next-token)))
        (cond ((or (equal? token (SchemeToken:.RPAREN))
                   (equal? token (SchemeToken:.EOFTOK)))
               (reverse elements))
              ((equal? token (SchemeToken:.DOT))
               (if (or proper? (= (length elements) 0))
                   (error-handler "Misplaced '.'" token-offset token-length)
                   (let ((element (begin (consume) (read1))))
                     (append (reverse elements) element))))
              (else
               (loop (cons (read1) elements)))))))
 
  
  (define (read1)
    (let ((token (get-next-token)))
      (cond ((equal? token (SchemeToken:.EOFTOK))
             #f)

            ;; -- lists
            ((equal? token (SchemeToken:.LPAREN))
             (read-list))

            ;; -- vectors
            ((equal? token (SchemeToken:.VECTORPREFIX))
             (read-vector))

            ;; -- right parenthesis (error case)
            ((equal? token (SchemeToken:.RPAREN))
             (error-handler "Misplaced closing parenthesis" token-offset token-length))

            ((equal? token (SchemeToken:.CONSTANT))
             (read-constant))

            ((equal? token (SchemeToken:.QUOTE))
             (read-quote/unquote/quasiquote 'quote))

            ((equal? token (SchemeToken:.UNQUOTE))
             (read-quote/unquote/quasiquote 'unquote))

            ((equal? token (SchemeToken:.UNQUOTE_SPLICING))
             (read-quote/unquote/quasiquote 'unquote-splicing))

            ((equal? token (SchemeToken:.BACKQUOTE))
             (read-quote/unquote/quasiquote 'quasiquote))
            
            ((equal? token (SchemeToken:.STRING))
             (read-string))
            
            ((equal? token (SchemeToken:.EXPR_COMMENT_PREFIX))
             (consume)
             (read1)
             (read1))

            ;; TODO support keywords, specials, etc.

            ((equal? token (SchemeToken:.SPECIAL))
             (let ((obj (make-syntax-object 'special token-offset token-length
                                            (SchemeScanner:getText scanner token-offset token-length))))
               (consume)
               obj))
            
            ;; -- anything else...
            (else
             (let ((obj (make-syntax-object 'symbol token-offset token-length 
                                            (SchemeScanner:getText scanner token-offset token-length))))
               (consume)
               obj)))))
  
  (init)
  (read1))


(define (stx-read-all document #!optional (stx-processor #f) (error-handler #f))
  (let ((error-handler (or error-handler
                           (lambda (message offset length)
                             (throw 'syntax-error message offset length)))))
   (let loop ((offset 0) (objs '()))
    (let ((obj (stx-read document offset error-handler)))
      (when stx-processor
        (stx-processor obj))
      (if obj
          (loop (+ (stx-object-offset obj) (stx-object-length obj))
                (cons obj objs))
          (reverse objs))))))

