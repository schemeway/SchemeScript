;;;
;;;; Pattern-matching on syntax objects
;;;
;;
;; @created   "Mon Jun 12 13:49:29 EDT 2006"
;; @author    "Dominique Boucher"
;;
;; This is a very simple-minded pattern-matcher for the 
;; Scheme editor syntax objects.


(define (unquote-pattern? pattern)
  (and (list? pattern)
       (= (length pattern) 2)
       (symbol? (cadr pattern))
       (eq? 'unquote (car pattern))))


(define (predicate-pattern? pattern)
  (and (list? pattern)
       (= (length pattern) 2)
       (eq? '? (car pattern))
       (symbol? (caddr pattern))))


(define (stx-match-pattern stx-obj pattern proc)
  (define variable-values '())
  
  (define (add-variable-value val)
    (set! variable-values (cons val variable-values)))
  
  (define (fail) (throw 'no-match))
  
  (define (match-pairs stx-objs pattern)
    (cond ((unquote-pattern? pattern)
           (add-variable-value stx-objs))
          ((and (pair? stx-objs) (pair? pattern))
           (match-pattern (car stx-objs) (car pattern))
           (match-pairs (cdr stx-objs) (cdr pattern)))
          ((and (null? stx-objs) (null? pattern))
           #t)
          (else
           (fail))))
  
 
  (define (match-pattern stx-obj pattern)
    (cond ((unquote-pattern? pattern)
           (add-variable-value stx-obj))
          
          ((pair? pattern)
           (if (stx-list? stx-obj)
               (match-pairs (stx-object-data stx-obj) pattern)
               (fail)))
          
          ((eq? '_ pattern)
           #t)
          
          ((and (symbol? pattern) (stx-symbol? stx-obj))
           (or (equal? pattern (stx-object-data stx-obj))
               (fail)))
          
          (else
           (fail)
;;           (error (format #f "invalid pattern: ~S" pattern))
           )))
  
  (catch 'no-match
    (lambda ()
      (match-pattern stx-obj pattern)
      (apply proc (reverse variable-values)))
    (lambda (key . vals)
      #f)))


(define (extract-pattern-variables pattern)
   ;; TODO: process vectors
  (define variables '())
  
  (define (add-variable v)
    (set! variables (cons v variables)))
  
  (define (process-pairs pattern)
    (cond ((unquote-pattern? pattern)
           (add-variable (cadr pattern)))
          ((pair? pattern)
           (process-pattern (car pattern))
           (process-pairs (cdr pattern)))))
  
  (define (process-pattern pattern)
    (cond ((unquote-pattern? pattern)
           (add-variable (cadr pattern)))
          ((pair? pattern)
           (process-pairs pattern))))
  
  (process-pattern pattern)
  (reverse variables))


(define-macro (stx-match obj . clauses)
  (let ((obj-var (gentemp)))
    `(let ((,obj-var ,obj))
       (or ,@(map (lambda (clause)
                    `(stx-match-pattern
                      ,obj-var
                      ',(car clause)
                      (lambda ,(extract-pattern-variables (car clause))
                        ,@(cdr clause))))
                  clauses)))))

