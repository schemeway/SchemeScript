;;;
;;;; Scheme Wrappers for some common widgets
;;;
;;
;; @created   "Thu Mar 10 11:26:42 EST 2005"
;; @author    "Dominique Boucher"
;; @copyright "NuEcho Inc."
;;


(define (default-shell)
  (try-catch 
      (IWorkbenchWindow:getShell (IWorkbench:getActiveWorkbenchWindow (PlatformUI:getWorkbench)))
    (exception <java.lang.Throwable> #!null)))


(define (default-display)
  (try-catch
      (IWorkbench:getDisplay (PlatformUI:getWorkbench))
    (exception <java.lang.Throwable> #!null)))


(define (message-box title message)
  (let ((shell (default-shell)))
    (MessageDialog:openInformation shell title message)))


;;;
;;;; --
;;;; Widgets
;;;


(define (new-label parent text #!key (style 0) (layout-data #f))
  (let ((label (Label:new parent style)))
    (Label:setText label text)
    (when layout-data
      (Label:setLayoutData button layout-data))
    label))


(define (new-button parent on-select #!key (image #f) (text #f) (style (SWT:.PUSH)) (layout-data #f))
  (let ((button (Button:new parent style)))
    (when text  (Button:setText button text))
    (when image (Button:setImage button image))
    (when on-select 
      (Button:addSelectionListener
       button
       (object (<org.eclipse.swt.events.SelectionAdapter>)
         ((widgetSelected (event :: <org.eclipse.swt.events.SelectionEvent>)) :: <void>
          (on-select this)))))
    (when layout-data
      (Button:setLayoutData button layout-data))
    button))


(define (new-text parent #!key (style 0) (value #f) (layout-data #f))
  (let ((text (Text:new parent style)))
    (when value (Text:setText text (as <String> value)))
    (when layout-data (Text:setLayoutData text layout-data))
    text))


(define (new-list parent #!key (style 0) (layout-data #f) (on-select #f) (items #f))
  (let ((widget (List:new parent style)))
    (when layout-data (List:setLayoutData widget layout-data))
    (when on-select
      (List:addSelectionListener 
       widget
       (object (<org.eclipse.swt.events.SelectionAdapter>)
         ((widgetSelected (event :: <org.eclipse.swt.events.SelectionEvent>)) :: <void>
          (let* ((indices :: <int[]> (List:getSelectionIndices widget))
                 (n ((primitive-array-length <int>) indices))
                 (i n)
                 (selection '()))
            (while (> i 0)
              (set! i (- i 1))
              (set! selection (cons ((primitive-array-get <int>) indices i) selection)))
            (on-select this selection))))))
    (when items
      (List:setItems widget (list->array (map (cut as <String> <>) items) <String>)))
    widget))


(define (canvas parent 
                #!key
                (on-move #f)
                (on-mouse-down #f)
                (on-mouse-up #f)
                (on-double-click #f)
                (on-paint #f))
  (let ((the-canvas (Canvas:new parent (SWT:.NO_BACKGROUND))))
    (when on-move
      (Canvas:addMouseMoveListener 
       the-canvas
       (object (<Object> <org.eclipse.swt.events.MouseMoveListener>)
         ((mouseMove (event :: <org.eclipse.swt.events.MouseEvent>)) :: <void>
          (on-move the-canvas (field event 'x) (field event 'y))))))
    
    (when on-mouse-down
      (Canvas:addMouseListener
       the-canvas
       (object (<org.eclipse.swt.events.MouseAdapter>)
         ((mouseDown (event :: <org.eclipse.swt.events.MouseEvent>)) :: <void>
          (on-mouse-down the-canvas (field event 'x) (field event 'y))))))
    
    (when on-mouse-up
      (Canvas:addMouseListener
       the-canvas
       (object (<org.eclipse.swt.events.MouseAdapter>)
         ((mouseUp (event :: <org.eclipse.swt.events.MouseEvent>)) :: <void>
          (on-mouse-up the-canvas (field event 'x) (field event 'y))))))
    
    (when on-double-click
      (Canvas:addMouseListener
       the-canvas
       (object (<org.eclipse.swt.events.MouseAdapter>)
         ((mouseDoubleClick (event :: <org.eclipse.swt.events.MouseEvent>)) :: <void>
          (on-double-click the-canvas (field event 'x) (field event 'y))))))
    
    (when on-paint
      (Canvas:addPaintListener
       the-canvas
       (object (<org.eclipse.swt.events.PaintListener>)
         ((paintControl (event :: <org.eclipse.swt.events.PaintEvent>)) :: <void>
          (on-paint the-canvas
                    (field event 'gc)
                    (field event 'x) (field event 'y)
                    (field event 'width) (field event 'height))))))

    the-canvas))


;;;
;;;; --
;;;; Graphics Context
;;;


(define (with-gc drawable proc)
  (let ((gc (GC:new drawable)))
    (try-finally
        (proc gc)
      (GC:dispose gc))))


(define-syntax with-fg-color
  (syntax-rules ()
    ((_ (gc color) body ...)
     (let ((old-color (GC:getForeground gc)))
       (GC:setForeground gc color)
       (try-finally
           (begin body ...)
         (GC:setForeground gc old-color))
       #!void))))


(define-syntax with-bg-color
  (syntax-rules ()
    ((_ (gc color) body ...)
     (let ((old-color (GC:getBackground gc)))
       (GC:setBackground gc color)
       (try-finally
           (begin body ...)
         (GC:setBackground gc old-color))
       #!void))))


(define-syntax with-buffer-gc
  (syntax-rules ()
    ((_ old-gc (gc width height) body ...)
     (let* ((image (Image:new #!null width height))
            (gc    (GC:new image)))
       (try-finally
           (begin
             body ...
             (GC:drawImage old-gc image 0 0))
         (begin
           (GC:dispose gc)
           (Image:dispose image)))))))

;;;
;;;; --
;;;; Layouts
;;;



(define (form-data #!key (left #f) (right #f) (top #f) (bottom #f) (width #f) (height #f))
  (let ((data (FormData:new)))
    (when left   (slot-set! data 'left left))
    (when right  (slot-set! data 'right right))
    (when top    (slot-set! data 'top top))
    (when bottom (slot-set! data 'bottom bottom))
    (when width  (slot-set! data 'width width))
    (when height (slot-set! data 'height height))
    data))


(define (form-attachment control (offset :: <int>))
  (if (number? control)
      (let ((numerator :: <int> control))
        (FormAttachment:new numerator offset))
      (let ((control :: <org.eclipse.swt.widgets.Control> control))
        (FormAttachment:new control offset))))


(define (grid-layout #!key (columns #f) (equal-columns #f) (margin-width #f) (margin-height #f) (hspacing #f) (vspacing #f))
  (let ((layout (GridLayout:new)))
    (when columns       (slot-set! layout 'numColumns columns))
    (when equal-columns (slot-set! layout 'makeColumnsEqualWidth #t))
    (when margin-width  (slot-set! layout 'marginWidth margin-width))
    (when margin-height (slot-set! layout 'marginHeight margin-height))
    (when hspacing      (slot-set! layout 'horizontalSpacing hspacing))
    (when vspacing      (slot-set! layout 'verticalSpacing vspacing))
    layout))


(define (grid-data #!key (style '()) (width #f) (height #f))
  (let* ((int-flag (grid-data-flags->integer style))
         (data     (GridData:new int-flag)))
    (when width (slot-set! data 'widthHint width))
    (when height (slot-set! data 'heightHint height))
    data))


(define *grid-constants*
  (list (cons 'fill-both       (GridData:.FILL_BOTH))
        (cons 'top             (GridData:.VERTICAL_ALIGN_BEGINNING))
        (cons 'bottom          (GridData:.VERTICAL_ALIGN_END))
        (cons 'vcenter         (GridData:.VERTICAL_ALIGN_CENTER))
        (cons 'vfill           (GridData:.VERTICAL_ALIGN_FILL))
        (cons 'left            (GridData:.HORIZONTAL_ALIGN_BEGINNING))
        (cons 'right           (GridData:.HORIZONTAL_ALIGN_END))
        (cons 'hcenter         (GridData:.HORIZONTAL_ALIGN_END))
        (cons 'hfill           (GridData:.HORIZONTAL_ALIGN_FILL))
        (cons 'grab-vertical   (GridData:.GRAB_VERTICAL))
        (cons 'grab-horizontal (GridData:.GRAB_HORIZONTAL))))


(define (grid-data-flags->integer flag-list) :: <int>
  (apply logior
         (map (lambda (flag) 
                (let ((p (assq flag *grid-constants*)))
                  (when (not p)
                    (error "invalid GridData flag: " flag))
                  (cdr p)))
              flag-list)))

