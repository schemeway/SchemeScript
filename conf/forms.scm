;;;
;;;; Form processors for most Kawa special forms
;;;
;;
;; @created   "Wed Sep 29 11:55:18 EDT 2004"
;; @author    "Dominique Boucher"
;;


(require 'srfi-1)


;;;
;;;; --
;;;; Helper functions
;;;


(define (add-entry! dictionary name description type resource line-number)
  (UserDictionary:addEntry dictionary (SymbolEntry:new name description type resource line-number 5)))


(define (get-line-number form default-value)
  (if (instance? form <gnu.lists.PairWithPosition>)
      (PairWithPosition:getLine form)
      default-value))


(define (signature->formals lst)
  (let loop ((lst lst))
    (if (pair? lst)
        (let ((parameter (car lst)))
          (if (and (pair? parameter) (symbol? (car parameter)))
              (cons (car parameter) (loop (cdr lst)))
              (cons parameter (loop (cdr lst)))))
        lst)))


;;;
;;;; --
;;;; Form processors
;;;


;;;
;;;;   define
;;;


(let ((define-processor 
       (lambda (dictionary form resource line-number)
         (cond ((and (pair? (cdr form)) (symbol? (cadr form)))
                (let* ((name        (cadr form))
                       (description (format #f "~a - variable" name)))
                  (add-entry! dictionary name description 'variable resource line-number)))
               ((and (pair? (cdr form)) (pair? (cadr form)) (symbol? (caadr form)))
                (let ((name        (caadr form))
                      (description (format #f "~a - procedure" (signature->formals (cadr form)))))
                  (add-entry! dictionary name description 'function resource line-number)))))))
  (define-form-processor 'define          define-processor)
  (define-form-processor 'define-private  define-processor)
  (define-form-processor 'define-constant define-processor))


;;;
;;;;   define-syntax, defmacro
;;;


(define-form-processor
 'define-syntax
 (lambda (dictionary form resource line-number)
   (when (and (pair? (cdr form))
              (symbol? (cadr form)))
     (let* ((name        (cadr form))
            (description (format #f "~a - user syntax" name)))
       (add-entry! dictionary name description 'syntax resource line-number)))))


(define-form-processor
 'defmacro
 (lambda (dictionary form resource line-number)
   (when (and (list? form)
              (>= (length form) 3)
              (symbol? (cadr form)))
     (let* ((name        (cadr form))
            (pattern     (caddr form))
            (description (format #f "~a - user syntax" (cons name pattern))))
       (add-entry! dictionary name description 'syntax resource line-number)))))


;;;
;;;;   module-name
;;;


(define-form-processor
 'module-name
 (lambda (dictionary form resource line-number)
   (when (and (pair? (cdr form))
              (null? (cddr form))
              (symbol? (cadr form)))
     (let* ((name        (cadr form))
            (description (format #f "~a - Kawa module" name)))
       (add-to-module-registry! resource name)
       (add-entry! dictionary name description 'module resource line-number)))))


;;;
;;;;   define-simple-class, define-class
;;;


(let ((class-processor
       (lambda (dictionary form resource line-number)
         (when (and (list? form)
                    (>= (length form) 3)
                    (symbol? (cadr form))
                    (list? (caddr form)))
           (let* ((class-name (cadr form))
                  (class-description (format #f "~a - Class" class-name)))
             (add-entry! dictionary class-name class-description 'class resource line-number)
             (for-each (lambda (field-or-method)
                         (cond 
                          ;; -- add an entry for a field
                          ((and (pair? field-or-method) 
                                (symbol? (car field-or-method)))
                           (let* ((field-name  (car field-or-method))
                                  (description (format #f "~a - field in ~a" field-name class-name))
                                  (line        (get-line-number field-or-method line-number)))
                             (add-entry! dictionary field-name description 'class resource line)))
                          ;; -- add an entry for a method
                          ((and (pair? field-or-method) 
                                (pair? (car field-or-method))
                                (symbol? (caar field-or-method)))
                           (let* ((method-name (caar field-or-method))
                                  (method-form (cons 'invoke 
                                                     (cons 'o 
                                                           (cons (string-append "'" (symbol->string method-name))
                                                                 (signature->formals (cdr (car field-or-method)))))))
                                  (description (format #f "~a - method in ~a" method-form class-name))
                                  (line        (get-line-number field-or-method line-number)))
                             (add-entry! dictionary method-name description 'class resource line)))))
                       (cdddr form)))))))
  (define-form-processor 'define-simple-class class-processor)
  (define-form-processor 'define-class class-processor))


;;;
;;;;   define-record-type
;;;


(define-form-processor 
 'define-record-type
 (lambda (dictionary form resource line-number)
   (when (and (list? form)
              (>= (length form) 4)
              (symbol? (cadr form))
              (list? (caddr form))
              (symbol? (cadddr form)))
     (let ((constructor (caddr form))
           (predicate   (cadddr form))
           (fields      (cddddr form)))
       ;; -- add an entry for the constructor
       (when (and (pair? constructor) (symbol? (car constructor)))
         (let ((name        (car constructor))
               (description (format #f "~a - record constructor" constructor))
               (line        (get-line-number constructor line-number)))
           (add-entry! dictionary name description 'constructor resource line)))
       ;; -- add an entry for the predicate
       (let ((description (format #f "(~a obj) - record predicate" predicate))
             (line        (get-line-number (cdddr form) line-number)))
         (add-entry! dictionary predicate description 'record-predicate resource line))
       ;; -- add an entry for each field
       (for-each (lambda (field-descriptor)
                   (when (and (list? field-descriptor)
                              (<= 2 (length field-descriptor) 3)
                              (every symbol? field-descriptor))
                     (let* ((getter-name (cadr field-descriptor))
                            (description (format #f "(~a record) - field getter" getter-name))
                            (line        (get-line-number field-descriptor line-number)))
                       (add-entry! dictionary getter-name description 'record-getter resource line)
                       (when (= (length field-descriptor) 3)
                         (let* ((setter-name (caddr field-descriptor))
                                (description (format #f "(~a record value) - field setter" setter-name)))
                           (add-entry! dictionary setter-name description 'record-getter resource line))))))
                 fields)))))


;;;
;;;;   define-namespace
;;;


(define-form-processor
 'define-namespace
 (lambda (dictionary form resource line-number)
   (when (namespace-form? form)
     (let* ((namespace-symbol    (cadr form))
            (namespace-string    (symbol->string namespace-symbol))
            (qualified-classname (caddr form)))
       (when (and (>= (string-length qualified-classname) 6)
                  (string=? "class:" (substring qualified-classname 0 6)))
         (let* ((classname  (substring qualified-classname 6 (string-length qualified-classname)))
                (signatures (find-class-methods classname)))
           (when signatures
             (for-each (lambda (name/signature)
                         (let ((description (string->symbol (format #f "(~a:~a)" namespace-string (cadr name/signature))))
                               (entry-name  (string->symbol (format #f "~a:~a" namespace-string (car name/signature)))))
                           (add-entry! dictionary entry-name description 'function resource line-number)))
                       signatures))
           (let ((namespace-description (string->symbol (format #f "~a - namespace" namespace-symbol))))
             (add-entry! dictionary namespace-symbol namespace-description 'namespace resource line-number))))))))


(define (namespace-form? form)
  (and (list? form)
       (= (length form) 3)
       (symbol? (cadr form))
       (string? (caddr form))))


(define (find-class-methods class-name)
  (or (find-project-class-methods (classname->pathname class-name))
      (find-internal-class-methods class-name)))


(define *workspace*      :: <org.eclipse.core.resources.IWorkspace>
  (RsrcPlugin:getWorkspace))
(define *workspace-root* :: <org.eclipse.core.resources.IWorkspaceRoot>
  (IWorkspace:getRoot *workspace*))
(define *java-model*     :: <org.eclipse.jdt.core.IJavaModel>
  (JavaCore:create *workspace-root*))


(define (find-project-class-methods class-name)
  (let loop ((projects (array->list (IJavaModel:getJavaProjects *java-model*))))
    (if (null? projects)
        #f
        (let ((project (car projects)))
          (if (instance? project <org.eclipse.jdt.core.IJavaProject>)
              (let* ((unit (or (find-project-element project (string-append class-name ".class"))
                               (find-project-element project (string-append class-name ".java"))))
                     (type (and unit
                                (instance? unit <org.eclipse.jdt.core.ICompilationUnit>)
                                (ICompilationUnit:findPrimaryType unit))))
                (if type
                    (map (lambda (method)
                           (imethod-signature method))
                         (filter (lambda (method)
                                   (or (IType:isInterface type)
                                       (Flags:isPublic (IMember:getFlags method))))
                                 (array->list (IType:getMethods type))))
                    (loop (cdr projects))))
              (loop (cdr projects)))))))


(define (imethod-signature method)
  (list (if (IMethod:isConstructor method) 'new (IMethod:getElementName method))
        (call-with-output-string 
         (lambda (port)
           (format port "~a" (if (IMethod:isConstructor method) "new" (IMethod:getElementName method)))
           (when (not (or (IMethod:isConstructor method) 
                          (Flags:isStatic (IMember:getFlags method))))
             (format port " self"))
           (for-each (lambda (name typename)
                       (format port " ~a :: <~a>" name typename))
                    (array->list (IMethod:getParameterNames method))
                    (map signature->string
                         (array->list (IMethod:getParameterTypes method))))))))


(define (signature->string signature)
  (Signature:toString (make <string> (as <String> signature))))


(define (find-internal-class-methods class-name)
  (try-catch 
      (let* ((class   (java.lang.Class:forName class-name))
             (methods (filter (lambda (method)
                                (Modifier:isPublic (Method:getModifiers method)))
                              (array->list (Class:getDeclaredMethods class))))
             (ctors   (filter (lambda (constructor)
                                (Modifier:isPublic (Constructor:getModifiers constructor)))
                              (array->list (Class:getConstructors class))))
             (names   (append (map (lambda (constructor)
                                     (method-signature 'new
                                                       (Constructor:getModifiers constructor)
                                                       (Constructor:getParameterTypes constructor)
                                                       #t))
                                   ctors)
                              (map (lambda (method) 
                                     (method-signature (Method:getName method)
                                                       (Method:getModifiers method)
                                                       (Method:getParameterTypes method)
                                                       #f))
                                   methods))))
        names)
    (exception <java.lang.ClassNotFoundException> 
               #f)
    (exception <java.lang.Throwable>
               (SchemePlugin:logException "Internal error!" exception)
               #f)))


(define (find-project-element project name)
  (let ((element (IJavaProject:findElement project (Path:new name))))
    (if (eq? element #!null)
        #f
        element)))


(define (method-signature name modifiers parameter-types constructor?)
  (list name 
        (call-with-output-string
         (lambda (port)
           (format port "~a" name)
           (when (not (or (Modifier:isStatic modifiers) constructor?))
             (format port " self"))
           (for-each (lambda (param-type)
                       (format port " <~a>" (type->signature param-type)))
                     (array->list parameter-types))))))


(define (type->signature cls)
  (if (Class:isArray cls)
      (string-append (type->signature (Class:getComponentType cls)) "[]")
      (let* ((clsname   :: <String> (Class:getName cls))
             (dot-index :: <int>    (String:lastIndexOf clsname (char->integer #\.))))
        (make <string>
          (as <String>
              (if (>= dot-index 0)
                  (String:substring clsname (+ dot-index 1))
                  clsname))))))

