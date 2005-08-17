;;;
;;;; Namespaces and types expansion
;;;
;;
;; @created   "Mon Mar 07 09:18:01 EST 2005"
;; @author    "Dominique Boucher"
;; @copyright "NuEcho Inc."
;;
;; 


(require 'srfi-1)


(define-simple-class <SimpleSearchCollector> (<Object> <org.eclipse.jdt.core.search.IJavaSearchResultCollector>)
  (elements)
  
  ((aboutToStart) :: <void>
   (set! elements '()))
  
  ((done) :: <void>
   (set! elements (reverse! elements)))
  
  ((accept (resource :: <org.eclipse.core.resources.IResource>)
           (start :: <int>)
           (end :: <int>)
           (element :: <org.eclipse.jdt.core.IJavaElement>)
           (accurracy :: <int>)) :: <void>
   (set! elements (cons element elements)))
  
  ((getProgressMonitor) :: <org.eclipse.core.runtime.IProgressMonitor>
   #!null))


(define (find-symbol-types symbol)
  (let ((engine  (SearchEngine:new))
        (pattern (SearchEngine:createSearchPattern symbol 0 0 #t))
        (scope   (SearchEngine:createWorkspaceScope))
        (collector (make <SimpleSearchCollector>)))
    (SearchEngine:search engine (RsrcPlugin:getWorkspace) pattern scope collector)
    (field collector 'elements)))


(define (find-package-types symbol)
  (let ((engine (SearchEngine:new))
        (pattern (SearchEngine:createSearchPattern (string-append symbol ".*") 0 0 #t))
        (scope (SearchEngine:createWorkspaceScope))
        (collector (make <SimpleSearchCollector>)))
    (SearchEngine:search engine (RsrcPlugin:getWorkspace) pattern scope collector)
    (field collector 'elements)))



(define (namespace-expander symbol)
  (let ((types (find-symbol-types symbol)))
    (if (pair? types)
        (let ((choosen-type-name (choose-type types)))
          (and choosen-type-name
               (format #f "(define-namespace ~a \"class:~a\")~%" symbol choosen-type-name)))
        #f)))


(define (typename-expander symbol)
  (let ((types (find-symbol-types symbol)))
    (if (pair? types)
        (let ((choosen-type-name (choose-type types)))
          (and choosen-type-name
               (format #f "<~a>" choosen-type-name)))
        #f)))


(define (package-expander symbol)
  (let ((types (find-package-types symbol)))
    (and (pair? types)
         (call-with-output-string
          (lambda (port)
            (for-each (lambda (type)
                        (format port "(define-namespace ~a \"class:~a\")~%" 
                                (IType:getElementName type)
                                (IType:getFullyQualifiedName type)))
                      types))))))


(define (choose-type types)
  (cond ((null? types)       #f)
        ((null? (cdr types)) (IType:getFullyQualifiedName (car types)))
        (else
         (choose-from-list "Type is ambiguous"
                           "Choose the right type:"
                           (delete-duplicates!
                            (map (lambda (type) (IType:getFullyQualifiedName type))
                                 types))))))


(define (make-symbol-expander expander)
  (lambda ()
    (let-values (((start end) (%backward-sexp)))
      (when (and start end (eq? (sexp-type start) symbol:))
        (with-buffer-text 
         start end 
         (lambda (text)
           (let ((clause (expander text)))
             (when clause
               (set-point start)
               (run-compound-change
                (lambda ()
                  (delete-text (- end start))
                  (insert-text clause)))))))))))


(define-constant expand-namespace (make-symbol-expander namespace-expander))
(define-constant expand-typename  (make-symbol-expander typename-expander))
(define-constant expand-package   (make-symbol-expander package-expander))

