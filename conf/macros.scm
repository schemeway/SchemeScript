;;;
;;;; Macros
;;;
;;
;; @created   "Tue Mar 08 09:02:09 EST 2005"
;; @author    "Dominique Boucher"
;; @copyright "NuEcho Inc."
;;


;;(module-name <org.schemeway.schemescript.macros>)
;;(module-static #t)


(define-syntax while
  (syntax-rules ()
    ((while expr body ...)
     (let loop () 
       (when expr
         body ...
         (loop))))))

