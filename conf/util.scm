;;;
;;;; Utility functions
;;;
;;
;; @created   "Mon Mar 07 09:11:32 EST 2005"
;; @author    "Dominique Boucher"
;; @copyright "NuEcho Inc."
;;


(define (array->list (array :: <Object[]>))
  (let ((array-get (primitive-array-get <Object>)))
    (let loop ((index :: <int> (- ((primitive-array-length <Object>) array) 1))
               (result '()))
      (if (>= index 0)
          (loop (- index 1) (cons (array-get array index) result))
          result))))


(define (list->array (lst :: <list>) #!optional (type <Object>))
  (let* ((len   :: <int> (length lst))
         (array          ((primitive-array-new type) len))
         (array-set      (primitive-array-set type))
         (index :: <int> 0))
    (for-each (lambda (elt)
                (array-set array index elt)
                (set! index (+ index 1)))
              lst)
    array))


(define (classname->pathname classname)
  (string-replace classname #\. #\/))


(define (string-replace str (ch-src :: <char>) (ch-dst :: <char>))
  (symbol->string (invoke (string->symbol str) 'replace ch-src ch-dst)))

