;;;
;;;; Arc-specific configurations
;;;
;;
;; @created   "Tue Feb 05 12:33:50 EST 2008"
;;


(def-indentation-rule 'def 'with 2)
(def-indentation-rule 'defset 'with 2)
(def-indentation-rule 'defmemo 'with 2)
(def-indentation-rule 'mac 'with 2)
(def-indentation-rule 'defop 'with 2)
(def-indentation-rule 'deftem 'with 1)
(for-each (cut def-indentation-rule <> 'with 1) '(w/uniq w/table w/outstring w/stdout w/stdin))
(for-each (cut def-indentation-rule <> 'with 2) '(w/infile w/outfile w/instring w/appendfile))



(def-constant 'nil)
(def-define  '= 'def 'mac 'defmemo 'set 'defset 'defop 'deftem)
(def-special '++ '-- 'and 'or 'atlet 'after 'in 'rfn 'afn 'fn 'compose
             'complement 'w/uniq 'atomic 'atwith 'atwiths
             'each 'do1 'caselet 'case 
             'loop 'on 'or= 'repeat 'push 'pull 'pop 'swap 'rotate 'pushnew 
             'zap 'nil! 't! 'iflet 'aif 'awhen 'aand 'accum 'drain 'default
             'w/infile 'w/outfile 'w/instring 'w/outstring 'w/appendfile 
             'w/stdout 'w/stdin 'tostring 'fromstring 'rand-choice
             'n-of 'insort 'insortnew 'summing 'obj 'time 'jtime 
             'time10 'errsafe 'until 'nor 'conswhen 'prf
             'w/table 'noisy-each 'point 'catch 'w/bars
             'help 'for 'forlen 'when 'whenlet 'while 'whiler 'whilet 'with 'withs)


(define-code-walker '(module)
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ ,name ,base-language . ,body)
       (recurse body)))))


;;;
;;;; Code walkers
;;;


(define-code-walker '(def defset defmemo)
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ ,name ,args . ,body)
       (when (stx-symbol? name)
         (let ((proto (cl-prototype (stx-object->datum name) (stx-object->datum args) 'procedure)))
           (parameterize ((current-dictionary-entry (new-dictionary-entry resource name 'procedure proto)))
             (recurse body))))))))


(define-code-walker '(mac)
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ ,name ,args . ,body)
       (when (stx-symbol? name)
         (let ((proto (cl-prototype (stx-object->datum name) (stx-object->datum args) 'macro)))
           (parameterize ((current-dictionary-entry (new-dictionary-entry resource name 'macro proto)))
             (recurse body))))))))


(define-code-walker '(= set)
  (lambda (stx resource recurse)
    (stx-match stx
      ((_ . ,bindings)
       (let loop ((bindings bindings))
         (if (and (pair? bindings)
                  (pair? (cdr bindings)))
             (let ((name (car bindings))
                   (val  (cadr bindings)))
                 (when (and (stx-symbol? name) (eq? #!null (current-dictionary-entry)))
                   (parameterize ((current-dictionary-entry (new-dictionary-entry resource name 'variable (symbol-description name))))
                                 (recurse val)))
               (loop (cddr bindings)))))))))


