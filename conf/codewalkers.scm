;;;
;;;; Code walkers
;;;
;;
;; @created   "Wed Jul 12 15:26:16 EDT 2006"
;;


(define current-dictionary-entry (make-parameter #!null))


;;;
;;;; Helpers
;;;


(define (new-dictionary-entry resource stx-obj category description #!optional (name #f))
  (let* ((name  (or name (stx-object-data stx-obj)))
         (entry (SymbolEntry:new name description category resource (stx-object-offset stx-obj) (stx-object-length stx-obj) 0)))
    (SymbolEntry:setParent entry (current-dictionary-entry))
    (add-dictionary-entry name entry)
    entry))


;;;
;;;; Entry point 
;;;


(define (walk-definitions resource stx-object)
  
  (define (recurse obj)
    (cond ((stx-object? obj)
           (scan-object obj))
          ((and (list? obj)
                (every stx-object? obj))
           (scan-objects obj))))
  
  (define (scan-objects stx-objs)
    (let loop ((stx-objs stx-objs))
      (cond ((pair? stx-objs)
             (scan-object (car stx-objs))
             (loop (cdr stx-objs)))
            (else
             (when (not (null? stx-objs))
               (scan-object stx-objs))))))
  
  (define (scan-object stx-obj)
    (stx-match stx-obj
      ((,rator . ,rands)
       (if (stx-symbol? rator)
           (let* ((sym    (stx-object-data rator))
                  (walker (get-code-walker sym)))
             (when walker
               (walker stx-obj resource recurse)))
           (begin
             (scan-object rator)
             (scan-objects rands))))
      
      (_ #t)))
  
  (scan-object stx-object))


;;;
;;;; Code walkers
;;;


(define *code-walkers* (make-hash-table))

(define (get-code-walker symbol)
  (hash-table-ref/default *code-walkers* symbol #f))


(define (define-code-walker symbol-or-symbols proc)
  (let ((symbols (if (list? symbol-or-symbols) symbol-or-symbols (list symbol-or-symbols))))
    (for-each (lambda (symbol)
                (hash-table-set! *code-walkers* symbol proc))
              symbols)))





