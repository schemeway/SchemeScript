;;;
;;;; Macros
;;;
;;
;; @created   "Tue Mar 08 09:02:09 EST 2005"
;; @author    "Dominique Boucher"
;; @copyright "NuEcho Inc."
;;


(define-syntax while
  (syntax-rules ()
    ((while expr body ...)
     (let loop () (when expr (begin body ... (loop)))))))

