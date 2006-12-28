;;;
;;;; Scheme dictionaries
;;;
;;
;; @created   "Tue Dec 26 15:57:18 EST 2006"
;;


;;;
;;;; --
;;;; Default dictionaries
;;;


(define *user-dictionary* (make-hash-table))


(define current-dictionary (make-parameter *user-dictionary*))


(define (dictionary-entry name proto type #!optional (level :: <int> 2) (dictionary (current-dictionary)))
  (add-dictionary-entry name (SymbolEntry:new name proto type level) dictionary))


;;;
;;;; --
;;;; User dict API
;;;


(define (add-dictionary-entry name entry #!optional (dictionary (current-dictionary)))
  (let ((current-entries (hash-table-ref/default dictionary name '())))
    (set-dictionary-entries! name (cons entry current-entries) dictionary)))


(define (set-dictionary-entries! name entries #!optional (dictionary (current-dictionary)))
  (hash-table-set! dictionary name entries))


(define (get-dictionary-entries name #!optional (dictionary (current-dictionary)))
  (synchronize-dictionary)
  (hash-table-ref/default dictionary name '()))


(define (get-dictionary-entries-for-resource resource #!optional (dictionary (current-dictionary)))
  (synchronize-dictionary)
  (hash-table-fold dictionary
                   (lambda (key value result)
                     (append (filter (lambda (entry) 
                                       (equal? resource (SymbolEntry:getFile entry)))
                                     value)
                             result))
                   '()))


(define (remove-dictionary-entries-for-resources resources #!optional (dictionary (current-dictionary)))
  (let ((names (hash-table-keys dictionary)))
    (for-each (lambda (name)
                (let ((new-entries (filter (lambda (entry) 
                                             (not (member (SymbolEntry:getFile entry) resources)))
                                           (get-dictionary-entries name))))
                  (set-dictionary-entries! name new-entries)))
              names)))


(define (find-completions prefix #!optional (dictionary (current-dictionary)))
  (synchronize-dictionary)
  (hash-table-fold dictionary
                   (lambda (key value result)
                     (if (starts-with prefix key)
                         (append value result)
                         result))
                   '()))


(define (update-dictionary-for-file (file :: <org.eclipse.core.resources.IFile>))
  (remove-dictionary-entries-for-resources (list file))
  (with-document-from-file file
    (lambda (document)
      (scan-resource file document))))


(define (scan-resource resource document)
  (clear-markers resource)
  (catch 'syntax-error
    (lambda ()
      (stx-read-all document (lambda (stx-obj) (walk-definitions resource stx-obj))))
    (lambda (key message offset len)
      (add-marker/offset! 'error resource (+ 1 (IDocument:getLineOfOffset document offset)) offset (+ offset len) message)
      #f)))


(define (synchronize-dictionary)
  (let ((updater (SchemeScriptPlugin:getDictionaryUpdater)))
    (*:processPendingResources updater #t)))


;;;
;;;; --
;;;; Now load the default dictionaries
;;;


(begin
  (load-relative "r5rs.scm")
  (load-relative "kawa.scm"))

