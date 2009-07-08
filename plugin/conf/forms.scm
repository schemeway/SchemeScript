;;;
;;;; Form processors for most Kawa and Scheme special forms
;;;
;;
;; @created   "Wed Sep 29 11:55:18 EDT 2004"
;; @author    "Dominique Boucher"
;;

;;;
;;;; --
;;;; Code walkers
;;;


(define-code-walker '(define* define define-private define-constant)
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ (,name . ,args) . ,body)
       (when (stx-symbol? name)
         (parameterize ((current-dictionary-entry (new-dictionary-entry resource name 'procedure (callable-description name))))
           (recurse body))))
      
      ((_ ,var :: ,type ,val)
       (when (stx-symbol? var)
         (parameterize ((current-dictionary-entry (new-dictionary-entry resource var 'variable (symbol-description var))))
           (recurse val))))
      
      ((_ ,var ,val)
       (when (stx-symbol? var)
         (parameterize ((current-dictionary-entry (new-dictionary-entry resource var 'variable (symbol-description var))))
           (recurse val)))))))


(define (symbol-description var #!optional (type 'variable))
  (format #f "~a - ~a" (stx-object->datum var) type))


(define (callable-description name-stx #!optional (type 'procedure))
  (let* ((signature (stx-object->datum (stx-object-parent name-stx))))
    (format #f "~a - ~a" (signature->formals signature) type)))

(define (cl-prototype name args type)
  (format #f "~a - ~a" (signature->formals (cons name args)) type))


(define (signature->formals lst)
  (let loop ((lst lst))
    (if (pair? lst)
        (let ((parameter (car lst)))
          (if (and (pair? parameter) (symbol? (car parameter)))
              (cons (car parameter) (loop (cdr lst)))
              (cons parameter (loop (cdr lst)))))
        lst)))

(let* ((type-description    (lambda (typename) (format #f "~a - record type" typename)))
       (ctor-description    (lambda (name-stx) (callable-description name-stx 'record-constructor)))
       (pred-description    (lambda (name-stx) (format #f "(~a obj) - record predicate" (stx-object->datum name-stx))))
       (getter-descrtiption (lambda (name-stx) (format #f "(~a obj) - record field getter" (stx-object->datum name-stx))))
       (setter-description  (lambda (name-stx) (format #f "(~a obj val) - record field setter" (stx-object->datum name-stx))))
      
       (process-record-fields
       (lambda (resource typename field-accessors)
         (for-each (lambda (field-accessor)
                     (stx-match field-accessor
                       ((,field-name ,field-getter)
                        (when (stx-symbol? field-getter)
                          (new-dictionary-entry resource field-getter 'field-getter (getter-descrtiption field-getter))))
                       ((,field-name ,field-getter ,field-setter)
                        (when (stx-symbol? field-getter)
                          (new-dictionary-entry resource field-getter 'field-getter (getter-descrtiption field-getter)))
                        (when (stx-symbol? field-setter)
                          (new-dictionary-entry resource field-setter 'field-setter (setter-description field-setter))))))
                   field-accessors))))

  (define-code-walker 'define-record-type
    (lambda (stx resource recurse)
      (stx-match stx
        ((_ ,typename (,cstor-name . ,field-names) ,predicate-name . ,field-accessors)
         (when (and (stx-symbol? typename) (stx-symbol? cstor-name) (stx-symbol? predicate-name))
           (let ((typename-symbol (stx-object->datum typename)))
             (parameterize ((current-dictionary-entry (new-dictionary-entry resource typename 'record-type (type-description typename-symbol))))
               (new-dictionary-entry resource cstor-name 'record-constructor (ctor-description cstor-name))
               (new-dictionary-entry resource predicate-name 'record-predicate (pred-description predicate-name))
               (process-record-fields resource typename-symbol field-accessors)))))))))


(define-code-walker 'define-syntax
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ (,name . ,args) . ,body)
       (when (stx-symbol? name)
         (new-dictionary-entry resource name 'user-syntax (callable-description name 'user-syntax))))
      ((_ ,name . ,body)
       (when (stx-symbol? name)
         (new-dictionary-entry resource name 'user-syntax (symbol-description name 'user-syntax)))))))


(define-code-walker '(define-macro define-macro*)
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ (,name . ,args) . ,body)
       (when (stx-symbol? name)
         (new-dictionary-entry resource name 'user-syntax (callable-description name 'user-syntax))))
      ((_ ,name . ,body)
       (when (stx-symbol? name)
         (new-dictionary-entry resource name 'user-syntax (symbol-description name 'user-syntax)))))))


(define-code-walker '(defmacro)
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ ,name ,args . ,body)
       (when (stx-symbol? name)
         (let ((proto (cl-prototype (stx-object->datum name) (stx-object->datum args) 'macro)))
           (parameterize ((current-dictionary-entry (new-dictionary-entry resource name 'macro proto)))
             (recurse body))))))))


(define-code-walker 'package*
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ ,name . ,rest)
       (when (stx-symbol? name)
         (add-to-module-registry! resource (stx-object-data name))
         (new-dictionary-entry resource name '|Snow! package| (symbol-description name '|Snow! package|)))))))


(define-code-walker 'module-name
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ ,name)
       (when (stx-symbol? name)
         (add-to-module-registry! resource (stx-object-data name))
         (new-dictionary-entry resource name 'module (symbol-description name 'module)))))))


(define-code-walker 'define-alias
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ ,name ,full-name)
       (when (and (stx-symbol? name)
                  (typename? (stx-object-data name))
                  (stx-symbol? full-name)
                  (typename? (stx-object-data full-name)))
         (new-dictionary-entry resource name 'alias (symbol-description name 'alias)))))))


(define (typename? obj)
  (and (symbol? obj)
       (let ((str (symbol->string obj)))
         (>= (string-length str) 2)
         (and (char=? #\< (string-ref str 0))
              (char=? #\> (string-ref str (- (string-length str) 1)))))))


(define-code-walker 'library
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ ,name . ,forms)
       (when (r6rs-library-name? name)
         (let ((library-name (string->symbol (format #f "~a" (stx-object->datum name)))))
           (parameterize ((current-dictionary-entry
                           (new-dictionary-entry resource name 'r6rs-library (symbol-description name '|R6RS library|) library-name)))
             (recurse forms))))))))


(define (r6rs-library-name? stx-name)
  (and (stx-list? stx-name)
       (every (lambda (obj) (or (symbol? obj) (list? obj)))
              (stx-object->datum stx-name))))


;;;
;;;; * define-simple-class, define-class
;;;

(let ((class-description (lambda (clsname) (format #f "~a - class" (stx-object->datum clsname)))))
  (define-code-walker '(define-simple-class)
    (lambda (stx resource recurse)
      (stx-match stx
        ((_ ,clsname (,super . ,supers) . ,fields-and-methods)
         (when (stx-symbol? clsname)
           (parameterize ((current-dictionary-entry (new-dictionary-entry resource clsname 'class (class-description clsname))))
             (for-each (lambda (field-or-method)
                         (stx-match field-or-method
                           (((,method-name . ,args) . ,body)
                            (new-dictionary-entry resource method-name 'method (callable-description method-name 'method)))
                           ((,field-name . ,field-facets)
                            (new-dictionary-entry resource field-name 'field (symbol-description field-name 'field)))))
                       fields-and-methods))))))))


;;;
;;;; * define-namespace
;;;

(define *schemescript:namespace-table* (HashMap:new))

(define (add-namespace namespace classname)
  (HashMap:put *schemescript:namespace-table* namespace classname))

(define (get-namespace-class namespace)
  (let ((value (HashMap:get *schemescript:namespace-table* namespace)))
    (and (not (eq? value #!null))
         value)))

(define (method-names prefix)
  (let ((prefix-parts (let ((parts (array->list (String:split prefix ":"))))
                        (cond ((= (length parts) 2)  
                               parts)
                              ((String:endsWith prefix ":")
                               (list (String:substring prefix 0 (- (String:length prefix) 1)) '||))
                              (else
              
                               '())))))
    (or (and (= (length prefix-parts) 2)
             (let* ((namespace (car prefix-parts))
                    (classname (or (get-namespace-class namespace) (symbol->string namespace)))
                    (members   (find-class-methods classname)))
               (and members
                    (map (lambda (member)
                           (SymbolEntry:new (car member) (cadr member) 'java-member))
                         (filter (lambda (member)
                                   (String:startsWith (car member) prefix))
                                 (map (lambda (member)
                                        (list (as <String> (format #f "~a:~a" namespace (car member)))
                                              (as <String> (cadr member))))
                                      members))))))
        '())))

(define-code-walker '(define-namespace)
  (lambda (stx resource recurse)
    (let ((form (stx-object->datum stx)))
      (when (namespace-form? form)
        (let* ((name-stx (cadr (stx-object-data stx)))
               (namespace-symbol (cadr form))
               (classname        (namespace->fqn (caddr form))))
          (new-dictionary-entry resource name-stx 'namespace (symbol-description name-stx 'namespace) namespace-symbol classname)
          (add-namespace namespace-symbol classname))))))


(define (namespace-form? form)
  (and (list? form)
       (= (length form) 3)
       (symbol? (cadr form))
       (let ((clsname (caddr form)))
         (or (and (string? clsname)
                  (>= (string-length clsname) 6)
                  (string=? "class:" (substring clsname 0 6)))
             (typename? clsname)))))


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

