;;;
;;;; Bundle-related definitions
;;;
;;
;; @created   "Thu Apr 06 13:51:37 EDT 2006"
;; @author    "Dominique Boucher"
;; @copyright "(c) 2006 Nu Echo Inc. All rights reserved."
;;


(define-namespace Platform         "class:org.eclipse.core.runtime.Platform")
(define-namespace Bundle           "class:org.osgi.framework.Bundle")

;; The platform (win32, linux, etc.)
(define *platform* (Platform:getOS))

;; The architecture
(define *architecture* (Platform:getOSArch))

;; The path separator
(define *path-separator*  
  (cond ((equal? (Platform:getOS) 'win32) ";")
        (else                             ":")))


;; Returns the absolute filename of a bundle entry, if it exists
;; @param bundle-name a symbol
;; @param entry a string
;; @returns the filename (a string) or #f
(define (locate-bundle-entry bundle-name entry)
  (let* ((bundle     (find-bundle bundle-name)))
    (and bundle
         (let ((entry (Bundle:getEntry bundle entry)))
           (and (not (eq? entry #!null))
                (let ((url-string (symbol->string (Platform:asLocalURL entry))))
                  (cond ((and (equal? *platform* 'win32) (starts-with "file:/" url-string))
                         (substring url-string 6 (string-length url-string)))
                        ((starts-with "file:" url-string)
                         (substring url-string 5 (string-length url-string)))
                        (else
                         url-string))))))))


;; Finds the given bundle by name
;; @param name a symbol
;; @returns a bundle object (Bundle) or #f
(define (find-bundle name)
  (let ((bundle (Platform:getBundle name)))
    (and (not (eq? bundle #!null)) bundle)))

