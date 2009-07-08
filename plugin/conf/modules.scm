;;;
;;;; Kawa module support
;;;
;;
;; @created   "Mon Mar 07 13:32:14 EST 2005"
;; @author    "Dominique Boucher"
;; @copyright "NuEcho Inc."
;;


;;;
;;;; * Global registry
;;;


(define *module-registry* (HashMap:new))


;;;
;;;; --
;;;; Public interface
;;;


(define (add-to-module-registry! resource modulename)
  (HashMap:put *module-registry* resource modulename))


(define (find-module-name resource)
  (let ((value (HashMap:get *module-registry* resource)))
    (if (eq? value #!null)
        #f
        value)))


(define (symbol->module-name symbol #!optional (buffer (current-buffer)))
  (let ((entries    (filter symbol?
                            (delete-duplicates!
                             (map (lambda (entry)
                                    (find-module-name (SymbolEntry:getFile entry)))
                                  (get-dictionary-entries symbol))))))

    (cond ((null? entries) #f)
          ((null? (cdr entries)) (car entries))
          (else 
           (choose-from-list "Symbol multiply declared" "Choose the module to require:" entries)))))


(define (add-require-clause #!optional (symbol (symbol-near-point)) (buffer (current-buffer)))
  (let ((modulename (symbol->module-name symbol buffer)))
    (when modulename
      (let* ((insertion (format #f "(require ~A)" modulename)))
        (let-values (((point needs-newline-before) (find-best-require-insertion-point modulename buffer)))
          (when point
            (let* ((line   (offset-line point buffer))
                   (offset (line-offset line buffer)))
              (when (< offset point)
                (try-finally 
                    (set! offset (line-offset (+ line 1) buffer))
                  #f))
              (insert-text (format #f "~a~a~%"  (if needs-newline-before "\n" "") insertion) offset buffer))))))))


(define (find-best-require-insertion-point insertion buffer)
  (let* ((start (first-non-comment-line-offset buffer))
         (start (skip-module-headers buffer start)))
    (skip-require-clauses buffer start insertion)))


(define (skip-module-headers buffer start)
  (let loop ((start start))
    (let-values (((sexp-start sexp-end) (%forward-sexp start buffer)))
      (if (or (looking-at "(module-name " sexp-start buffer)
              (looking-at "(module-static " sexp-start buffer)
              (looking-at "(module-extends " sexp-start buffer)
              (looking-at "(module-export " sexp-start buffer))
          (loop sexp-end)
          start))))


(define (skip-require-clauses buffer start name)
  (let loop ((start start) (needs-newline #t))
    (let-values (((sexp-start sexp-end) (%forward-sexp start buffer)))
      (if (looking-at "(require " sexp-start buffer)
          (let-values (((name-start name-end) (%forward-sexp (+ sexp-start 9) buffer)))
            (let ((text (buffer-text name-start name-end buffer)))
              (cond ((string=? name text)
                     (values #f #f))
                    ((string<? name text)
                     (values sexp-start #f))
                    (else
                     (loop sexp-end #f)))))
          (values start needs-newline)))))


(define (first-non-comment-line-offset buffer)
  (let ((line 0))
    (try-finally
        (while (looking-at ";" (line-offset line buffer) buffer)
          (set! line (+ line 1)))
      #f)
    (line-offset line buffer)))

