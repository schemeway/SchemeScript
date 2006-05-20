;;;
;;;; Circles examples, in SISC
;;;
;;
;; @created   "Thu May 18 09:10:09 EDT 2006"
;; @author    "Dominique Boucher"
;; @copyright "(c) 2006 Nu Echo Inc. All rights reserved."
;;
;;
;; Note that the performance is not good as in Kawa.


(load "scratchpad-sisc.scm")


;;;
;;;; * Some colors
;;;


(define *triangle-color* (make-color 0 0 255))
(define *handle-color*   (make-color 196 0 0))
(define *circle-color*   (make-color 64 128 64))


;;;
;;;; * The Point data type
;;;


(define-generics get-point-x set-point-x! get-point-y set-point-y!)
(define-class (<point>)
  (x get-point-x set-point-x!)
  (y get-point-y set-point-y!))


(define-method (initialize (<point> p) (<number> x) (<number> y))
  (set-point-x! p x)
  (set-point-y! p y))


;;;
;;;; * The center of each circle
;;;


(define *p1* (make <point> 10 10))
(define *p2* (make <point> 30 100))
(define *p3* (make <point> 70 40))

(define *all-points* (list *p1* *p2* *p3*))


;;;
;;;; * Some Point-related functions
;;;


(define (square x) (* x x))

(define (distance p1 p2) 
  (sqrt (+ (square (- (get-point-x p2) (get-point-x p1)))
           (square (- (get-point-y p2) (get-point-y p1))))))


(define (move-point p delta-x delta-y)
  (set-point-x! p (+ (get-point-x p) delta-x))
  (set-point-y! p (+ (get-point-y p) delta-y)))


;;;
;;;; * The origin
;;;


(define *origin* (make <point> 0 0))

(define (move-origin delta-x delta-y)
  (move-point *origin* delta-x delta-y))



;;;
;;;; * Drawing functions
;;;


(define (draw-line gc p1 p2)
  (let ((origin-x (get-point-x *origin*))
        (origin-y (get-point-y *origin*)))
    (gc-draw-line gc
                  (->jint (- (get-point-x p1) origin-x))
                  (->jint (- (get-point-y p1) origin-y))
                  (->jint (- (get-point-x p2) origin-x))
                  (->jint (- (get-point-y p2) origin-y)))))


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
    (gc-draw-oval gc
                  (->jint (- (get-point-x point) radius origin-x))
                  (->jint (- (get-point-y point) radius origin-y))
                  (->jint width)
                  (->jint width))))


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
  (let* ((point      (make <point> (+ (get-point-x *origin*) x) (+ (get-point-y *origin*) y)))
         (candidates (filter (lambda (p) (<= (distance p point) 3)) *all-points*)))
    (if (pair? candidates)
        (car candidates)
        #f)))


;;;
;;;; * Viewer factory
;;;

(define-generics
 mouse-down? set-mouse-down!
 selected-point set-selected-point!
 old-x set-old-x!
 old-y set-old-y!)


(define-class (<triangle-viewer> <canvas>)
  (down? mouse-down? set-mouse-down!)
  (selected-point selected-point set-selected-point!)
  (old-x old-x set-old-x!)
  (old-y old-y set-old-y!))


(define-method (on-mouse-move (<triangle-viewer> viewer) (<number> x) (<number> y))
  (when (mouse-down? viewer)
    (if (selected-point viewer)
        (move-point (selected-point viewer) (- x (old-x viewer)) (- y (old-y viewer)))
        (move-origin (- (old-x viewer) x) (- (old-y viewer) y)))
    (set-old-x! viewer x)
    (set-old-y! viewer y)
    (redraw viewer)))


(define-method (on-mouse-down (<triangle-viewer> viewer) (<number> x) (<number> y))
  (when (not (mouse-down? viewer))
    (set-mouse-down! viewer #t)
    (set-selected-point! viewer (find-closest-point x y))
    (set-old-x! viewer x)
    (set-old-y! viewer y)
    (redraw viewer)))


(define-method (on-mouse-up (<triangle-viewer> viewer) (<number> x) (<number> y))
  (when (mouse-down? viewer)
    (set-mouse-down! viewer #f)
    (redraw viewer)))


(define-method (on-paint (<triangle-viewer> viewer) (<gc> gc) (<number> x) (<number> y) (<number> w) (<number> h))
  (with-buffer-gc gc (igc 1024 768)
    (draw-triangle igc)
    (draw-circles igc)))


;;;
;;;; * Main
;;;


;; Add the viewer to the Scratchpad view
(add-scratchpad-view 
 '|Triangles| 
 (lambda (parent)
   (widget-owner 
    (make <triangle-viewer> parent))))


(make <triangle-viewer> #f)

