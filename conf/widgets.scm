;;;
;;;; Scheme Wrappers for common widgets
;;;
;;
;; @created   "Thu Mar 10 11:26:42 EST 2005"
;; @author    "Dominique Boucher"
;; @copyright "NuEcho Inc."
;;

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


(define (new-button parent on-select #!key (image #f) (text #f) (style *SWT.PUSH*) (layout-data #f))
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


;;;
;;;; --
;;;; Layouts
;;;



(define (form-data #!key (left #f) (right #f) (top #f) (bottom #f) (width #f) (height #f))
  (let ((data :: <org.eclipse.swt.layout.FormData> (FormData:new)))
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
  (let ((layout :: <org.eclipse.swt.layout.GridLayout> (GridLayout:new)))
    (when columns       (slot-set! layout 'numColumns columns))
    (when equal-columns (slot-set! layout 'makeColumnsEqualWidth #t))
    (when margin-width  (slot-set! layout 'marginWidth margin-width))
    (when margin-height (slot-set! layout 'marginHeight margin-height))
    (when hspacing      (slot-set! layout 'horizontalSpacing hspacing))
    (when vspacing      (slot-set! layout 'verticalSpacing vspacing))
    layout))


(define (grid-data #!key (style '()) (width #f) (height #f))
  (let* ((int-flag :: <int> (grid-data-flags->integer style))
         (data :: <org.eclipse.swt.layout.GridData> (GridData:new int-flag)))
    (when width (slot-set! data 'widthHint width))
    (when height (slot-set! data 'heightHint height))
    data))


(define *grid-constants*
  (list (cons 'fill-both       (static-field <org.eclipse.swt.layout.GridData> 'FILL_BOTH))
        (cons 'top             (static-field <org.eclipse.swt.layout.GridData> 'VERTICAL_ALIGN_BEGINNING))
        (cons 'bottom          (static-field <org.eclipse.swt.layout.GridData> 'VERTICAL_ALIGN_END))
        (cons 'vcenter         (static-field <org.eclipse.swt.layout.GridData> 'VERTICAL_ALIGN_CENTER))
        (cons 'vfill           (static-field <org.eclipse.swt.layout.GridData> 'VERTICAL_ALIGN_FILL))
        (cons 'left            (static-field <org.eclipse.swt.layout.GridData> 'HORIZONTAL_ALIGN_BEGINNING))
        (cons 'right           (static-field <org.eclipse.swt.layout.GridData> 'HORIZONTAL_ALIGN_END))
        (cons 'hcenter         (static-field <org.eclipse.swt.layout.GridData> 'HORIZONTAL_ALIGN_END))
        (cons 'hfill           (static-field <org.eclipse.swt.layout.GridData> 'HORIZONTAL_ALIGN_FILL))
        (cons 'grab-vertical   (static-field <org.eclipse.swt.layout.GridData> 'GRAB_VERTICAL))
        (cons 'grab-horizontal (static-field <org.eclipse.swt.layout.GridData> 'GRAB_HORIZONTAL))))


(define (grid-data-flags->integer flag-list)
  (apply logior
         (map (lambda (flag) 
                (let ((p (assq flag *grid-constants*)))
                  (when (not p)
                    (error "invalid GridData flag: " flag))
                  (cdr p)))
              flag-list)))

