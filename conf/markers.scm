;;;
;;;; Markers interface
;;;
;;
;; @created   "Fri Mar 31 13:00:40 EST 2006"
;; @author    "Dominique Boucher"
;; @copyright "(c) 2006 Nu Echo Inc. All rights reserved."
;;


(define *error-type*       :: <int> (static-field <org.eclipse.core.resources.IMarker> 'SEVERITY_ERROR))
(define *warning-type*     :: <int> (static-field <org.eclipse.core.resources.IMarker> 'SEVERITY_WARNING))
(define *char-start-attr*  :: <symbol> (static-field <org.eclipse.core.resources.IMarker> 'CHAR_START))
(define *char-end-attr*    :: <symbol> (static-field <org.eclipse.core.resources.IMarker> 'CHAR_END))
(define *line-number-attr* :: <symbol> (static-field <org.eclipse.core.resources.IMarker> 'LINE_NUMBER))
(define *severity-attr*    :: <symbol> (static-field <org.eclipse.core.resources.IMarker> 'SEVERITY))
(define *text-attr*        :: <symbol> (static-field <org.eclipse.core.resources.IMarker> 'TEXT))
(define *message-attr*     :: <symbol> (static-field <org.eclipse.core.resources.IMarker> 'MESSAGE))


(define *marker-id* :: <symbol> 'org.schemeway.plugins.schemescript.error)


(define (level->marker-type level) :: <int>
  (case level
    ((error)   *error-type*)
    ((warning) *warning-type*)
    (else      *error-type*)))


(define (add-marker! level (file :: <org.eclipse.core.resources.IResource>) (line :: <int>) message)
  (let ((attr (Hashtable:new)))
    (Hashtable:put attr *message-attr*      (as <String> message))
    (Hashtable:put attr *text-attr*         (as <String> message))
    (Hashtable:put attr *severity-attr*     (let ((type :: <int> (level->marker-type level)))
                                              (java.lang.Integer:new type)))
    (Hashtable:put attr *line-number-attr*  (java.lang.Integer:new line))
    (MarkerUtilities:createMarker file attr *marker-id*)))


(define (clear-markers resource)
  (try-catch
      (IResource:deleteMarkers resource *marker-id* #t (static-field <org.eclipse.core.resources.IResource> 'DEPTH_INFINITE))
    (exception <java.lang.Throwable>
               (SchemePlugin:logException
                (format #f "Can't delete markers for '~a'" resource)
                exception))))

