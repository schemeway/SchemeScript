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


(define (add-entry! dictionary name description type resource line-number #!optional (parent #!null))
  (let ((entry (SymbolEntry:new name description type resource line-number 5)))
    (when (not (eq? parent #!null))
      (SymbolEntry:setParent entry parent))
    (UserDictionary:addEntry dictionary entry)
    entry))


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
                  (add-entry! dictionary name description 'procedure resource line-number)))))))
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
           (let* ((class-name        (cadr form))
                  (class-description (format #f "~a - Class" class-name))
                  (class-entry       (add-entry! dictionary class-name class-description 'class resource line-number)))
             (for-each (lambda (field-or-method)
                         (cond 
                          ;; -- add an entry for a field
                          ((and (pair? field-or-method) 
                                (symbol? (car field-or-method)))
                           (let* ((field-name  (car field-or-method))
                                  (description (format #f "~a - field in ~a" field-name class-name))
                                  (line        (get-line-number field-or-method line-number)))
                             (add-entry! dictionary field-name description 'field resource line class-entry)))
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
                             (add-entry! dictionary method-name description 'method resource line class-entry)))))
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
;;;;   define-alias
;;;

(define-form-processor
 'define-alias
 (lambda (dictionary form resource line-number)
   (when (alias-form? form)
     (let* ((name        (cadr form))
            (description (format #f "~a" name)))
       (add-entry! dictionary name description 'alias resource line-number)))))


(define (alias-form? form)
  (and (list? form)
       (= (length form) 3)
       (typename? (cadr form))
       (typename? (caddr form))))

;;;
;;;;   define-namespace
;;;


(define-form-processor
 'define-namespace
 (lambda (dictionary form resource line-number)
   (when (namespace-form? form)
     (let* ((namespace-symbol (cadr form))
            (classname        (namespace->fqn (caddr form)))
            (signatures       (find-class-methods classname)))
       (when signatures
         (for-each (lambda (name/signature)
                     (let ((description (string->symbol (format #f "(~a:~a)" namespace-symbol (cadr name/signature))))
                           (entry-name  (string->symbol (format #f "~a:~a" namespace-symbol (car name/signature)))))
                       (add-entry! dictionary entry-name description 'java-member resource line-number)))
                   signatures))))))


(define (namespace-form? form)
  (and (list? form)
       (= (length form) 3)
       (symbol? (cadr form))
       (let ((clsname (caddr form)))
         (or (and (string? clsname)
                  (>= (string-length clsname) 6)
                  (string=? "class:" (substring clsname 0 6)))
             (typename? clsname)))))


(define (typename? obj)
  (and (symbol? obj)
       (let ((str (symbol->string obj)))
         (>= (string-length str) 2)
         (and (char=? #\< (string-ref str 0))
              (char=? #\> (string-ref str (- (string-length str) 1)))))))

(define (namespace->fqn typename)
  (cond ((string? typename)
         (substring typename 6 (string-length typename)))
        ((symbol? typename)
         (let ((clsname (symbol->string typename)))
           (substring clsname 1 (- (string-length clsname) 1))))))

(define (find-class-methods class-name)
  (or (find-project-class-methods (classname->pathname class-name))
      (find-internal-class-methods class-name)))


(define-constant *workspace*      :: <org.eclipse.core.resources.IWorkspace>
  (RsrcPlugin:getWorkspace))
(define-constant *workspace-root* :: <org.eclipse.core.resources.IWorkspaceRoot>
  (IWorkspace:getRoot *workspace*))
(define-constant *java-model*     :: <org.eclipse.jdt.core.IJavaModel>
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
                    (append 
                     (map ifield-name
                          (filter (lambda (fld)
                                    (let ((flags (IMember:getFlags fld)))
                                      (Flags:isPublic flags)))
                                  (array->list (IType:getFields type))))
                     (map imethod-signature
                          (filter (lambda (method)
                                    (or (IType:isInterface type)
                                        (Flags:isPublic (IMember:getFlags method))))
                                  (array->list (IType:getMethods type)))))
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


(define (ifield-name fld)
  (let ((name (symbol->string (IField:getElementName fld))))
    (list (string->symbol (string-append "." name))
          (call-with-output-string
           (lambda (port)
             (display name port)
             (when (not (Flags:isStatic (IMember:getFlags fld)))
               (display " self" port)))))))


(define (signature->string signature)
  (Signature:toString (make <string> (as <String> signature))))


(define (find-internal-class-methods class-name)
  (try-catch 
      (let* ((class   (java.lang.Class:forName class-name))
             (flds    (filter (lambda (fld)
                                (Modifier:isPublic (Field:getModifiers fld)))
                              (array->list (Class:getFields class))))
             (methods (filter (lambda (method)
                                (Modifier:isPublic (Method:getModifiers method)))
                              (array->list (Class:getMethods class))))
             (ctors   (filter (lambda (constructor)
                                (Modifier:isPublic (Constructor:getModifiers constructor)))
                              (array->list (Class:getConstructors class))))
             (names   (append (list (list '.class "Class"))
                              (map (lambda (fld)
                                     (field-name fld))
                                   flds)
                              (map (lambda (constructor)
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


(define (field-name fld)
  (let ((name (Field:getName fld)))
    (list (string->symbol (format #f ".~a" name))
          (call-with-output-string 
           (lambda (port)
             (display name port)
             (let ((flags (Field:getModifiers fld)))
               (when (not (Modifier:isStatic flags))
                 (display " self" port))))))))


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

