;;;
;;;; Scheme dictionaries
;;;
;;
;; @created   "Tue Dec 26 15:57:18 EST 2006"
;;


(require 'srfi-69)

(define-namespace SymbolEntry "class:org.schemeway.plugins.schemescript.dictionary.SymbolEntry")


;;;
;;;; --
;;;; Default dictionaries
;;;


(define *r5rs-dictionary* (make-hash-table))
(define *kawa-dictionary* (make-hash-table))
(define *user-dictionary* (make-hash-table))

(define *all-dictionaries* (list *r5rs-dictionary* *kawa-dictionary* *user-dictionary*))


(define current-dictionary (make-parameter *user-dictionary*))


(define (dictionary-entry name proto type #!optional (level :: <int> 0) (dictionary (current-dictionary)))
  (add-dictionary-entry name (SymbolEntry:new name proto type level) dictionary))


;;;
;;;; --
;;;; User dict API
;;;


(define (add-dictionary-entry name entry #!optional (dictionary *user-dictionary*))
  (let ((current-entries (get-dictionary-entries name dictionary)))
    (set-dictionary-entries! name (cons entry current-entries) dictionary)))


(define (set-dictionary-entries! name entries #!optional (dictionary *user-dictionary*))
  (hash-table-set! dictionary name entries))


(define (get-dictionary-entries name #!optional (dictionary *user-dictionary*))
  (hash-table-ref/default dictionary name '()))


(define (get-dictionary-entries-for-resource resource #!optional (dictionary *user-dictionary*))
  (hash-table-fold dictionary
                   (lambda (key value result)
                     (append (filter (lambda (entry) 
                                       (not (eq? resource (SymbolEntry:getFile entry))))
                                     value)
                             result))
                   '()))

(define (remove-dictionary-entries-for-resources resources #!optional (dictionary *user-dictionary*))
  (let ((names (hash-table-keys dictionary)))
    (for-each (lambda (name)
                (let ((new-entries (filter (lambda (entry) 
                                             (not (memq (SymbolEntry:getFile entry) resources)))
                                           (get-dictionary-entries name))))
                  (set-dictionary-entries! name new-entries)))
              names)))


(define (find-completions prefix)
  (apply append! 
         (map (lambda (dictionary)
                (hash-table-fold dictionary
                                 (lambda (key value result)
                                   (if (starts-with prefix key)
                                       (append value result)
                                       result))
                                 '()))
              *all-dictionaries*)))


(define (add-completions-to-list (prefix :: <symbol>) (lst :: <java.util.List>))
  (for-each (cut java.util.List:add lst <>)
            (find-completions (symbol->string prefix))))


;;;
;;;; --
;;;; Now load the default dictionaries
;;;



(begin
  (parameterize ((current-dictionary *r5rs-dictionary*))
    (load-relative "r5rs.scm"))
  (parameterize ((current-dictionary *kawa-dictionary*))
    (load-relative "kawa.scm")))

