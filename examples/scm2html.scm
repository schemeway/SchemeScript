;;;
;;;; Puts the selected text in the ClipBoard in HTML form.
;;;
;;
;; @created   "Wed Jun 21 15:49:45 EDT 2006"
;; @author    "Dominique Boucher"
;; @copyright "(c) 2006 Nu Echo Inc. All rights reserved."
;;


(define-namespace Clipboard "class:org.eclipse.swt.dnd.Clipboard")
(define-namespace Transfer "class:org.eclipse.swt.dnd.Transfer")
(define-namespace TextTransfer "class:org.eclipse.swt.dnd.TextTransfer")
(define-namespace SchemeTextTools "class:org.schemeway.plugins.schemescript.editor.SchemeTextTools")
(define-namespace KeywordManager "class:org.schemeway.plugins.schemescript.parser.KeywordManager")


(define (put-text-in-clipboard str)
  (let* ((display   (Display:getCurrent))
         (clipboard (Clipboard:new display))
         (transfers (list->array (list (TextTransfer:getInstance)) <org.eclipse.swt.dnd.Transfer>))
         (data      (list->array (list (as <String> str)))))
    (Clipboard:setContents clipboard data transfers)
    (Clipboard:dispose clipboard)))


(define (sexp->html buffer start end)
  
  (define *keyword-manager* 
    (*:getKeywordManager
     (*:getTextTools 
      (SchemeScriptPlugin:getDefault))))

  (define scanner (SchemeScanner:new))

  (define (init-scanner document start end)
    (SchemeScanner:setRange scanner document start (- end start)))


  (define (scan-and-format-region port document start end)
    (init-scanner document start end)
    (let loop ()
      (let* ((token (SchemeScanner:nextToken scanner))
             (type  (SchemeToken:getType token)))
        (when (not (equal? type (SchemeToken:.EOFTOK)))
          (let* ((offset (SchemeToken:getOffset token))
                 (len    (SchemeToken:getLength token))
                 (text   (SchemeScanner:getText scanner offset len)))
            (format-token port type text))
          (loop)))))

  (define *basic-classes*
    (list (cons (SchemeToken:.COMMENT)  'cmt)
          (cons (SchemeToken:.CONSTANT) 'cst)
          (cons (SchemeToken:.STRING)   'str)
          (cons (SchemeToken:.SPECIAL)  'spc)
          (cons (SchemeToken:.RPAREN)   'paren)
          (cons (SchemeToken:.LPAREN)   'paren)))
  
  (define *symbol-classes*
    '((define . def)
      (keyword . kwd)
      (mutator . mut)
      (special . form)
      (constant . glob)))
  
  (define (format-token port type text)
    (cond ((assoc type *basic-classes*)
           => (lambda (p)
                (format-text port (cdr p) (escape-text text))))
          ((equal? type (SchemeToken:.SYMBOL))
           (format-symbol port (escape-text text)))
          (else
           (format port "~A" (escape-text text)))))
  
  (define (is-keyword? (txt :: <String>))
    (let ((len (*:length txt)))
      (and (> len 1)
           (or (and (char=? (*:charAt txt 0) #\:)
                    (= 0 (*:lastIndexOf txt #\:)))
               (and (char=? (*:charAt txt (- len 1)) #\:)
                    (= (- len 1) (*:lastIndexOf txt #\:)))))))
  
  (define (format-symbol port text)
    (cond ((assq (KeywordManager:getType *keyword-manager* text) *symbol-classes*)
           => (lambda (p)
                (format-text port (cdr p) (escape-text text))))
          ((keyword? text))
          (else
           (format port "~a" (escape-text text)))))
  
  (define (format-text port clazz text)
    (format port "<span class=\"~a\">~a</span>" clazz (escape-text text))) ;; TODO: escape < and >

  (define (escape-text text) 
    (*:replaceAll (*:replaceAll text ">" "&gt;") "<" "&lt;"))
  
  (call-with-output-string
   (lambda (port)
     (format port "<pre>\n")
     (scan-and-format-region port (SchemeEditor:getDocument buffer) start end)
     (format port "</pre>\n"))))


(define (format-code-to-clipboard #!optional (buffer (current-buffer)))
  (with-selection 
   (lambda (start end)
     (when (not (= start end))
       (put-text-in-clipboard
        (sexp->html buffer start end))))))

