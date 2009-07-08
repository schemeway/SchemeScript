;;;
;;;; Scratchpad access using SISC
;;;
;;
;; @created   "Wed May 17 10:26:09 EDT 2006"
;; @author    "Dominique Boucher"
;; @copyright "(c) 2006 Nu Echo Inc. All rights reserved."
;;

(import s2j)
(import oo)
(import generic-procedures)
(import type-system)
(require-library 'sisc/libs/srfi/srfi-1)
(import srfi-1)


;;;
;;;; * UI thread
;;;

(define-java-class <java.lang.runnable>)
(define-java-proxy (runnable thunk)
  (<java.lang.runnable>)
  (define run 
    (lambda (_)
      (thunk)
      (void))))

(define-generic-java-methods run)

(define-java-class <display> |org.eclipse.swt.widgets.Display|)
(define-generic-java-methods sync-exec)

(define *null-display* (java-null <display>))

(define (run-in-ui-thread thunk)
  (let* ((the-display (get-default *null-display*))
         (result      #f)
         (proc        (lambda ()
                        (set! result (thunk)))))
    (sync-exec the-display (runnable proc))
    result))


(define-java-class <scratchpad-view> |org.schemeway.plugins.schemescript.views.KawaScratchpadView|)
(define-generic-java-methods add-view get-control set-focus get-default)

(define (show-scratchpad)
  (run-in-ui-thread
   (lambda ()
     (let ((control (get-control (java-null <scratchpad-view>))))
       (set-focus control)))))


(define (add-scratchpad-view name factory)
  (run-in-ui-thread
   (lambda ()
     (let* ((parent (get-control (java-null <scratchpad-view>)))
            (view   (factory parent)))
       (add-view (java-null <scratchpad-view>) (->jstring name) view)))))


(define-java-class <native-control> |org.eclipse.swt.widgets.Control|)
(define-java-class <native-composite> |org.eclipse.swt.widgets.Composite|)
(define-java-class <native-canvas> |org.eclipse.swt.widgets.Canvas|)


(define-java-classes 
 (<swt>    |org.eclipse.swt.SWT|)
 (<color>  |org.eclipse.swt.graphics.Color|)
 (<device> |org.eclipse.swt.graphics.Device|)
 (<gc>     |org.eclipse.swt.graphics.GC|)
 (<image>  |org.eclipse.swt.graphics.Image|))

(define-generic-java-methods 
 dispose 
 get-foreground set-foreground
 get-background set-background
 (gc-draw-line |drawLine|) (gc-draw-oval |drawOval|) (gc-draw-image |drawImage|)
 (redraw/native redraw))

(define *swt* (java-null <swt>))

(define-generic-java-field-accessors
 (no-background |NO_BACKGROUND|))


(define *null-device* (java-null <device>))

(define (make-color r g b)
  (java-new <color> *null-device* (->jint r) (->jint g) (->jint b)))

(define (with-gc drawable proc)
  (let ((gc (java-new <gc> drawable)))
    (try-finally
        (proc gc)
      (dispose gc))))


(define-syntax with-fg-color
  (syntax-rules ()
    ((_ (gc color) body ...)
     (let ((old-color (get-foreground gc)))
       (set-foreground gc color)
       (dynamic-wind
        (lambda () (set-foreground gc color))
        (lambda () (begin body ...))
        (lambda () (set-foreground gc old-color)))))))


(define-syntax with-bg-color
  (syntax-rules ()
    ((_ (gc color) body ...)
     (let ((old-color (get-background gc)))
       (dynamic-wind 
        (lambda () (set-background gc color))
        (lambda () (begin body ...))
        (lambda () (set-background gc old-color)))))))


(define-syntax with-buffer-gc
  (syntax-rules ()
    ((_ old-gc (gc width height) body ...)
     (let* ((image (java-new <image> *null-device* (->jint width) (->jint height)))
            (gc    (java-new <gc> image)))
       (dynamic-wind
        (lambda () (void))
        (lambda ()
          body ...
          (gc-draw-image old-gc image (->jint 0) (->jint 0)))
        (lambda ()
          (dispose gc)
          (dispose image)))))))


(define-generics on-paint on-mouse-up on-mouse-down on-mouse-move on-double-click redraw)
(define-generics widget-owner set-widget-owner!)

(define-class (<widget>)
  (owner widget-owner set-widget-owner!))

(define-method (initialize (<widget> self) (<object> owner))
  (error "Can't instantiate abstract widget!"))

(define-method (on-paint (<widget> self) (<gc> gc) (<number> x) (<number> y) (<number> width) (<number> height))
  (void))

(define-method (on-mouse-up (<widget> self) (<number> x) (<number> y))
  (void))

(define-method (on-mouse-down (<widget> self) (<number> x) (<number> y))
  (void))

(define-method (on-mouse-move (<widget> self) (<number> x) (<number> y))
  (void))

(define-method (on-double-click (<widget> self) (<number> x) (<number> y))
  (void))

(define-method (redraw (<widget> self))
  (redraw/native (widget-owner self)))


(define-java-classes
 (<org.eclipse.swt.events.MouseMoveListener> |org.eclipse.swt.events.MouseMoveListener|)
 (<org.eclipse.swt.events.MouseListener>     |org.eclipse.swt.events.MouseListener|)
 (<org.eclipse.swt.events.PaintListener>     |org.eclipse.swt.events.PaintListener|)
 (<org.eclipse.swt.events.MouseEvent>        |org.eclipse.swt.events.MouseEvent|)
 (<org.eclipse.swt.events.PaintEvent>        |org.eclipse.swt.events.PaintEvent|))


(define-generic-java-field-accessors
 (.event-gc |gc|)
 (.event-x |x|)
 (.event-y |y|)
 (.event-width |width|)
 (.event-height |height|))

(define-java-proxy (mouse-move-listener widget)
  (<org.eclipse.swt.events.MouseMoveListener>)
  (define (mouse-move p event)
    (on-mouse-move widget (->number (.event-x event)) (->number (.event-y event)))))

(define-java-proxy (mouse-listener widget)
  (<org.eclipse.swt.events.MouseListener>)
  (define (mouse-double-click p event)
    (on-double-click widget (->number (.event-x event)) (->number (.event-y event))))
  (define (mouse-up p event)
    (on-mouse-up widget (->number (.event-x event)) (->number (.event-y event))))
  (define (mouse-down p event)
    (on-mouse-down widget (->number (.event-x event)) (->number (.event-y event)))))

(define-java-proxy (paint-listener widget)
  (<org.eclipse.swt.events.PaintListener>)
  (define (paint-control p event)
    (on-paint widget
              (.event-gc event)
              (->number (.event-x event))
              (->number (.event-y event))
              (->number (.event-width event))
              (->number (.event-height event)))))


(define-generic-java-methods add-mouse-listener add-mouse-move-listener add-paint-listener)

(define-class (<canvas> <widget>))

(define-method (initialize (<canvas> self) (<native-composite> parent))
  (let ((canvas (java-new <native-canvas> parent (no-background *swt*))))
    (set-widget-owner! self canvas)
    (add-mouse-move-listener canvas (mouse-move-listener self))
    (add-mouse-listener canvas (mouse-listener self))
    (add-paint-listener canvas (paint-listener self))))

