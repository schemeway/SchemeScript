;;;
;;;; Clojure-related definitions
;;;
;;
;; @created   "Mon Sep 07 21:41:23 EDT 2009"
;;

(def-indentation-rule 'def 'definition 0)
(def-indentation-rule 'defn 'definition 0)


(def-constant 'nil 'true 'false)
(def-define 'def 'defn)
(def-special 'var 'try 'catch 'throw 'finally 'loop 'recur)

(define-code-walker '(def)
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ ,var . ,rest)
       (when (stx-symbol? var)
         (parameterize ((current-dictionary-entry (new-dictionary-entry resource var 'variable (symbol-description var))))
                       (recurse rest)))))))

(define-code-walker '(defn)
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ ,name ,args . ,body)
       (when (stx-symbol? name)
         (let ((proto (cl-prototype (stx-object->datum name) (stx-object->datum args) 'procedure)))
           (parameterize ((current-dictionary-entry (new-dictionary-entry resource name 'procedure proto)))
             (recurse body))))))))

(define-code-walker '(defmulti)
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ ,name ,dispatch)
       (when (stx-symbol? name)
         (let ((proto (cl-prototype (stx-object->datum name) "[]" 'multi-method)))
           (parameterize ((current-dictionary-entry (new-dictionary-entry resource name 'procedure proto)))
             (recurse body))))))))
