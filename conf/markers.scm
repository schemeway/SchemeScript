;;;
;;;; Markers interface
;;;
;;
;; @created   "Fri Mar 31 13:00:40 EST 2006"
;; @author    "Dominique Boucher"
;; @copyright "(c) 2006 Nu Echo Inc. All rights reserved."
;;


;;;
;;;; * Interface
;;;



(define *error-type*       :: <int> (static-field <org.eclipse.core.resources.IMarker> 'SEVERITY_ERROR))
(define *warning-type*     :: <int> (static-field <org.eclipse.core.resources.IMarker> 'SEVERITY_WARNING))
(define *char-start-attr*  :: <symbol> (static-field <org.eclipse.core.resources.IMarker> 'CHAR_START))
(define *char-end-attr*    :: <symbol> (static-field <org.eclipse.core.resources.IMarker> 'CHAR_END))
(define *line-number-attr* :: <symbol> (static-field <org.eclipse.core.resources.IMarker> 'LINE_NUMBER))
(define *severity-attr*    :: <symbol> (static-field <org.eclipse.core.resources.IMarker> 'SEVERITY))
(define *text-attr*        :: <symbol> (static-field <org.eclipse.core.resources.IMarker> 'TEXT))
(define *message-attr*     :: <symbol> (static-field <org.eclipse.core.resources.IMarker> 'MESSAGE))


(define *error-marker-id* :: <symbol> 'org.schemeway.plugins.schemescript.error)
(define *task-marker-id* :: <symbol> 'org.schemeway.plugins.schemescript.task)


(define (level->marker-type level) :: <int>
  (case level
    ((error)   *error-type*)
    ((warning) *warning-type*)
    (else      *error-type*)))


(define (add-marker! level (file :: <org.eclipse.core.resources.IResource>) (line :: <int>) message #!optional (marker-type *error-marker-id*))
  (let ((attr (Hashtable:new)))
    (Hashtable:put attr *message-attr*      (as <String> message))
    (Hashtable:put attr *text-attr*         (as <String> message))
    (Hashtable:put attr *severity-attr*     (let ((type :: <int> (level->marker-type level)))
                                              (java.lang.Integer:new type)))
    (Hashtable:put attr *line-number-attr*  (java.lang.Integer:new line))
    (MarkerUtilities:createMarker file attr marker-type)))


(define (add-task! (file :: <org.eclipse.core.resources.IResource>) (start :: <int>) (end :: <int>) message)
  (let ((attr (Hashtable:new)))
    (Hashtable:put attr *message-attr*      (as <String> message))
    (Hashtable:put attr *text-attr*         (as <String> message))
    (Hashtable:put attr *char-start-attr*   (java.lang.Integer:new start))
    (Hashtable:put attr *char-end-attr*     (java.lang.Integer:new end))
    (MarkerUtilities:createMarker file attr *task-marker-id*)))


(define (clear-markers resource #!optional (marker-type *error-marker-id*))
  (try-catch
      (IResource:deleteMarkers resource marker-type #t (static-field <org.eclipse.core.resources.IResource> 'DEPTH_INFINITE))
    (exception <java.lang.Throwable>
               (SchemePlugin:logException
                (format #f "Can't delete markers for '~a'" resource)
                exception))))


(define (clear-tasks resource)
  (clear-markers resource *task-marker-id*))


;;;
;;;; * Tasks support
;;;


(define-namespace SchemeScanner "class:org.schemeway.plugins.schemescript.parser.SchemeScanner")
(define-namespace SchemeToken "class:org.schemeway.plugins.schemescript.parser.SchemeToken")
(define-namespace Pattern "class:java.util.regex.Pattern")
(define-namespace Matcher "class:java.util.regex.Matcher")


(define *task-pattern* (Pattern:compile "(FIXME|TODO)[ \\t]*(.*)$"))


(define (scan-tasks (buffer :: <org.schemeway.plugins.schemescript.editor.SchemeEditor>))
  (define (remove-tasks (file :: <org.eclipse.core.resources.IResource>))
    (let ((markers (array->list (*:findMarkers file *task-marker-id* #t 2))))
      (when (pair? markers)
        (*:deleteMarkers file *task-marker-id* #t 2))))
  
  (let ((file (SchemeEditor:getFile buffer)))
    (remove-tasks file)
    (let ((scanner (SchemeScanner:new))
          (document (*:getDocument buffer)))
      (*:setRange scanner document 0 (*:getLength document))
      (let loop ((token (*:nextToken scanner)))
        (when (not (eq? token (SchemeToken:.EOF)))
          (if (eqv? (SchemeToken:getType token) (SchemeToken:.COMMENT))
              (let* ((offset  (SchemeScanner:getTokenOffset scanner))
                     (len     (SchemeScanner:getTokenLength scanner))
                     (text    (SchemeScanner:getText scanner offset len))
                     (matcher (Pattern:matcher *task-pattern* text)))
                (when (*:find matcher)
                  (let ((start (*:start matcher))
                        (end   (*:end matcher)))
                    (add-task! file (+ offset start) (+ offset end) (*:substring text start end))))))
          (loop (*:nextToken scanner)))))))


(add-save-hook 'scan-tasks)