;;;
;;;; A simple illustration of the Kawa Scratchpad View
;;;
;;
;; @created   "Wed Aug 17 13:25:22 EDT 2005"
;; @author    "Schemeway"
;; @copyright "The SchemeWay Project"
;;

;; This simple program displays three circles tangent to each other, with
;; segments connecting their centers.
;; 
;;
;; To run, simply evaluate the buffer (typically by typing CTRL-SHIFT-L in the editor).
;; To move the center of one the circles, click and drag.
;; To move the entire construction, click and drag somewhere outside of the
;; three centers.

;;;
;;;; --
;;;; Implementation
;;;

;;;
;;;;   A few colors
;;;


(define *triangle-color* (Color:new #!null 0 0 255))
(define *handle-color*   (Color:new #!null 196 0 0))
(define *circle-color*   (Color:new #!null 64 128 64))

;;;
;;;;   The Point data type
;;;


(define-record-type Point
  (make-point x y)
  point?
  (x get-point-x set-point-x!)
  (y get-point-y set-point-y!))


;;;
;;;;   The center of each circle
;;;


(define *p1* (make-point 10 10))
(define *p2* (make-point 30 100))
(define *p3* (make-point 70 40))

(define *all-points* (list *p1* *p2* *p3*))


;;;
;;;;   Some Point-related functions
;;;


(define (square x) (* x x))

(define (distance p1 p2) 
  (sqrt (+ (square (- (get-point-x p2) (get-point-x p1)))
           (square (- (get-point-y p2) (get-point-y p1))))))


(define (move-point p delta-x delta-y)
  (set-point-x! p (+ (get-point-x p) delta-x))
  (set-point-y! p (+ (get-point-y p) delta-y)))


;;;
;;;;   The origin
;;;


(define *origin* (make-point 0 0))

(define (move-origin delta-x delta-y)
  (move-point *origin* delta-x delta-y))


;;;
;;;;   Drawing functions
;;;


(define (draw-line gc p1 p2)
  (let ((origin-x (get-point-x *origin*))
        (origin-y (get-point-y *origin*)))
    (GC:drawLine gc
                 (- (get-point-x p1) origin-x)
                 (- (get-point-y p1) origin-y)
                 (- (get-point-x p2) origin-x)
                 (- (get-point-y p2) origin-y))))


(define (draw-triangle gc)
  (with-fg-color (gc *triangle-color*)
   (draw-line gc *p1* *p2*)
   (draw-line gc *p2* *p3*)
   (draw-line gc *p3* *p1*))
  (with-fg-color (gc *handle-color*)
   (draw-circle gc *p1* 3)
   (draw-circle gc *p2* 3)
   (draw-circle gc *p3* 3)))


(define (draw-circle gc point radius)
  (let ((width    (* radius 2))
        (origin-x (get-point-x *origin*))
        (origin-y (get-point-y *origin*)))
    (GC:drawOval gc 
                 (- (get-point-x point) radius origin-x)
                 (- (get-point-y point) radius origin-y) width width)))


(define (draw-circles gc)
  (with-fg-color (gc *circle-color*)
  (let* ((a (distance *p1* *p2*))
         (b (distance *p1* *p3*))
         (c (distance *p2* *p3*))
         (p (/ (+ a b (- c)) 2))
         (q (- a p))
         (r (- c q)))
    (draw-circle gc *p1* p)
    (draw-circle gc *p2* q)
    (draw-circle gc *p3* r))))



(define (find-closest-point x y)
  (let* ((point      (make-point (+ (get-point-x *origin*) x) (+ (get-point-y *origin*) y)))
         (candidates (filter (lambda (p) (<= (distance p point) 3)) *all-points*)))
    (if (pair? candidates)
        (car candidates)
        #f)))


;;;
;;;;   Viewer factory
;;;


(define (triangle-viewer parent)
  (let ((down? #f) (selected-point #f) (old-x #f) (old-y #f))
    (canvas
     parent
     on-mouse-down: (lambda (canvas x y)
                      (when (not down?)
                        (set! down? #t)
                        (set! selected-point (find-closest-point x y))
                        (set! old-x x)
                        (set! old-y y)
                        (Canvas:redraw canvas)))
     on-mouse-up:   (lambda (canvas x y)
                      (when down?
                        (set! down? #f)
                        (Canvas:redraw canvas)))
     on-move:       (lambda (canvas x y)
                      (when down?
                        (if selected-point
                            (move-point selected-point (- x old-x) (- y old-y))
                            (move-origin (- old-x x) (- old-y y)))
                        (set! old-x x)
                        (set! old-y y)
                        (Canvas:redraw canvas)))
     on-paint:      (lambda (canvas gc x y w h)
                      (with-buffer-gc gc (igc 1024 768)
                        (draw-triangle igc)
                        (draw-circles igc))))))


;;;
;;;;   Main
;;;

;; Add the viewer to the Kawa Scratchpad view
(add-scratchpad-view 'Triangles triangle-viewer)

