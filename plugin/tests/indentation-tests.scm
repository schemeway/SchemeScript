(define x
  ;;
  `(foo (foo x)
        print
        ,x
        ,x))

;;
;; asdfasdf


(define (foo x . args) ; asdfasdf
  ;; sadfa
  'machin)


(foo a
     b
     c)

(a b
 c
 d)


`(a b c d e
  f g h i j)


("abc" "def"
 "ghi" "jkl"
 "mno" "pqr" "stu"
 "vw")

((foo)
 x y
 z)


`(,@(a)
  ,@(a))

(begin
  code code ; comment
            ; comment
  code)

(define print display)

(not-a-procedure ; a
                 ; b = right
 (print a1
                                        ; c = wrong
        a2 ; d
           ; e
        )
 (print
  a1
                                        ; f = wrong
                                        ; g = wrong
  )
 '(not-a-procedure abc
                                        ; h = wrong
   def ; i = right
       ; j = right
   )
 )

