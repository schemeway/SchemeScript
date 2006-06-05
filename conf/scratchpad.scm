;;;
;;;; Interface to the Kawa Scratchpad View
;;;
;;
;; @created   "Tue Mar 15 10:19:54 EST 2005"
;; @author    "Dominique Boucher"
;; @copyright "NuEcho Inc."
;;
;; The Kawa Scratchpad is a view that allows developers to test their SWT/JFace controls
;; without opening a new top-level shell window. 


(define-namespace KawaScratchpadView "class:org.schemeway.plugins.schemescript.views.KawaScratchpadView")
(define-namespace Control "class:org.eclipse.swt.widgets.Control")


;;;
;;;; * Interface to the Kawa scratchpad View
;;;

;; @function (add-scratchpad-view name factory)
;;   This function adds a new page to the Kawa Scratchpad View.
;; @param name the name of the view (a string or symbol)
;; @param factory a one argument function that creates the control to be placed
;;        as a new page in the Scratchpad view. The argument to the function is
;;        the parent Composite. The function must return a SWT Control object.
;; @return the control inserted in the Kawa Scratchpad View.
(define (add-scratchpad-view name factory)
  (let* ((parent  (KawaScratchpadView:getControl))
         (control (factory parent)))
    (KawaScratchpadView:addView name control)
    control))


;; @function (show-scratchpad)
;;   Displays the Kawa Scratchpad View
(define (show-scratchpad)
  (let ((control (KawaScratchpadView:getControl)))
    (Control:setFocus control)))

