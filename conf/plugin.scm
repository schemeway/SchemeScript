;;;
;;;; Plugin related functions
;;;
;;
;; @created   "Tue Feb 05 15:59:03 EST 2008"
;;

(define-namespace SchemeScriptPlugin <org.schemeway.plugins.schemescript.SchemeScriptPlugin>)
(define-namespace SchemeTextTools <org.schemeway.plugins.schemescript.editor.SchemeTextTools>)
(define-namespace SchemeIndentationManager <org.schemeway.plugins.schemescript.indentation.SchemeIndentationManager>)
(define-namespace IndentationRule <org.schemeway.plugins.schemescript.indentation.IndentationRule>)
(define-namespace KeywordManager <org.schemeway.plugins.schemescript.parser.KeywordManager>)


(define (schemescript-plugin)
  (SchemeScriptPlugin:getDefault))


(define (schemescript-preference-store)
  (SchemeScriptPlugin:getPreferenceStore (schemescript-plugin)))


(define (schemescript-indentation-manager)
  (SchemeTextTools:getIndentationManager 
   (SchemeScriptPlugin:getTextTools
    (schemescript-plugin))))


(define (reset-indentation-rules)
  (SchemeIndentationManager:loadRules schemescript-indentation-manager))


(define (def-indentation-rule sym category hint)
  (when (and (symbol? sym)
             (memq category '(default definition sequence if with none))
             (integer? 0)
             (>= hint 0))
    (let ((new-rule (IndentationRule:new sym category hint)))
      (SchemeIndentationManager:addIndentationRule (schemescript-indentation-manager) new-rule))))


(define (schemescript-keyword-manager)
  (SchemeTextTools:getKeywordManager
   (SchemeScriptPlugin:getTextTools
    (schemescript-plugin))))


(define (def-keyword . kwds)
  (for-each (cut KeywordManager:addKeyword (schemescript-keyword-manager) <>) kwds))

(define (def-special . names)
  (for-each (cut KeywordManager:addSpecial (schemescript-keyword-manager) <>) names))

(define (def-define  . names) 
  (for-each (cut KeywordManager:addDefine (schemescript-keyword-manager) <>) names))

(define (def-constant . objs) 
  (for-each (cut KeywordManager:addConstant (schemescript-keyword-manager) <>) objs))

(define (def-mutator . names)
  (for-each (cut KeywordManager:addMutator (schemescript-keyword-manager) <>) names))



