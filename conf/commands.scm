;;;
;;;; Various editing commands
;;;
;;
;; @created   "Mon Jul 10 13:28:47 EDT 2006"
;;
;; Most of the following editor commands have been heavily inspired by 
;; Taylor Campbell's excellent paredit.el, a superb minor Emacs mode for 
;; parentheses-based languages.


;; TODO: documentation


(define (raise-sexp)
  (let ((buffer (current-buffer))
        (point  (point)))
    (with-forward-sexp point buffer
     (lambda (start end)
       (with-buffer-text start end
        (lambda (sexp)
          (with-up-sexp start buffer
           (lambda (start-outer end-outer)
             (run-compound-change ;; make sure undo does the right thing
              (lambda ()
                (delete-text (- end-outer start-outer) start-outer buffer)
                (insert-text sexp start-outer buffer)
                (set-point start-outer buffer))
              buffer)))))))))


(define (wrap-sexp)
  (let ((buffer (current-buffer))
        (point  (point)))
    (with-forward-sexp point buffer
     (lambda (start end)
       (run-compound-change
        (lambda ()
          (insert-text ")" end buffer)
          (insert-text "(" start buffer)
          (set-point (+ start 1) buffer))
        buffer)))))


(define (splice-sexp)
  (let ((buffer (current-buffer))
        (point  (point)))
    (with-up-sexp point buffer
     (lambda (start end)
       (run-compound-change
        (lambda ()
          (delete-text 1 (- end 1) buffer)
          (delete-text 1 start buffer)
          (set-point (- point 1) buffer))
        buffer)))))


(define (split-sexp)
  (let ((buffer (current-buffer))
        (point  (point)))
    (with-up-sexp point buffer
     (lambda _ 
       (run-compound-change
        (lambda ()
          (remove-whitespaces)
          (insert-text ") (" point buffer)
          (set-point (+ point 1) buffer))
        buffer)))))


(define (join-sexp)
  (let ((buffer   (current-buffer))
        (point    (point)))
    (with-backward-sexp point buffer
     (lambda (start-before end-before)
       (with-forward-sexp point buffer
        (lambda (start-after end-after)
          (run-compound-change
           (lambda ()
             (delete-text 1 start-after buffer)
             (delete-text 1 (- end-before 1) buffer)
             (set-point (- point 1) buffer))
           buffer)))))))


(define (forward-slurp-sexp)
  (let ((buffer (current-buffer))
        (point  (point)))
    (with-up-sexp point buffer
     (lambda (start end)
       (with-forward-sexp end buffer
        (lambda (_ next-end)
          (run-compound-change
           (lambda ()
             (insert-text ")" next-end buffer)
             (delete-text 1 (- end 1) buffer)
             (set-point point buffer))
           buffer)))))))


(define (backward-slurp-sexp)
  (let ((buffer (current-buffer))
        (point  (point)))
    (with-up-sexp point buffer
     (lambda (start end)
       (with-backward-sexp start buffer
        (lambda (previous-start _)
          (run-compound-change
           (lambda ()
             (delete-text 1 start buffer)
             (insert-text "(" previous-start buffer)
             (set-point point buffer))
           buffer)))))))


(define (kill-next-sexp)
  (let ((buffer (current-buffer))
        (point (point)))
    (with-forward-sexp point buffer
     (lambda (start end)
       (run-compound-change
        (lambda ()
          (delete-text (- end point) point buffer)
          (set-point point)
          (remove-whitespaces))
        buffer)))))


(define (kill-previous-sexp)
  (let ((buffer (current-buffer))
        (point (point)))
    (with-backward-sexp point buffer
     (lambda (start end)
       (run-compound-change
        (lambda ()
          (delete-text (- point start) start buffer)
          (set-point start))
        buffer)))))


(define (comment-selection)
  (let ((buffer (current-buffer)))
    (with-selection
     (lambda (start end)
       (when (not (= start end))
         (run-compound-change 
          (lambda ()
            (insert-text "|#" end buffer)
            (insert-text "#|" start buffer))
          buffer)))
     buffer)))


(define (remove-whitespaces)
  (let ((buffer (current-buffer))
        (start  (point)))
    (let loop ((point start))
      (if (and (< point (point-max buffer))
               (char-whitespace? (char-at point buffer)))
          (loop (+ point 1))
          (if (> point start)
              (delete-text (- point start) start buffer))))))


(define (up-and-forward)
  (with-up-sexp (point) (current-buffer)
   (lambda (start end)
     (set-point end)
     (insert-text " ")
     (forward-char 1))))


(define (up-and-forward/newline)
  (with-up-sexp (point) (current-buffer)
   (lambda (start end)
     (set-point end)
     (insert-text "\n")
     (forward-char 1))))

