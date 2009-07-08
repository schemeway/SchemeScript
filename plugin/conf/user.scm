;;;
;;;; User initialization file
;;;
;;
;; @created   "Tue Mar 15 10:17:44 EST 2005"
;; @author    "Dominique Boucher"
;; @copyright "NuEcho Inc."
;;


;;;
;;;; * System initializations
;;;

(require 'srfi-1)
(require 'srfi-69)

;; -- DO NOT REMOVE THE FOLLOWING LINES --
(load-relative "macros.scm")
(load-relative "util.scm")
(load-relative "plugin.scm")
(load-relative "foreign.scm")
(load-relative "bundles.scm")
(load-relative "constants.scm")
(load-relative "buffer.scm")
(load-relative "commands.scm")
(load-relative "widgets.scm")
(load-relative "dialogs.scm")
(load-relative "namespaces.scm")
(load-relative "modules.scm")
(load-relative "markers.scm")
(load-relative "scratchpad.scm")
(load-relative "reader.scm")
(load-relative "stxmatch.scm")
(load-relative "codewalkers.scm")
(load-relative "forms.scm")
(load-relative "refactoring.scm")
(load-relative "dictionaries.scm")



;; -- ADD YOUR OWN INITIALIZATIONS HERE
;;;
;;;; * User initializations
;;;

;; uncomment the next line to enable Arc support
;(load-relative "languages/arc.scm")