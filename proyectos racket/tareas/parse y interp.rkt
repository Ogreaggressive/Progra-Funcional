#lang play
#|

Lenguaje AE -> Arithmetich Expression

1. Concrete syntax
2. Abstract syntax
3. parse
4. interp

(num 6)
(add (num 3) (num 4))
(add (num 3) (add (num 4) (num 1)))
(sub (num 5) (num 2))

<expr> ::=   (num <number?>)
           | (add <expr> <expr>)
           | (sub <expr> <expr>)

|#

(deftype Expr
  [num n]
  [add l r]
  [sub l r]
  [bool b]
  )

; interp : expr -> val
(define (interp expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp l) (interp r))]
    [(sub l r) (- (interp l) (interp r))]
    [(bool b) b]
    ))

(test (interp (num 6)) 6)
(test (interp (add (num 3) (num 4))) 7)
(test (interp (sub (num 5) (num 2))) 3)
(test (interp (bool #t)) #t)


#|

<Src> ::= <number?> | <boolean?>
         | {'+ <Src> <Src>}

|#

; parse : src -> expr
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(? boolean?) (bool src)]
    )
  )

(test (parse 6) (num 6))
(test (parse '{+ 3 4}) (add (num 3) (num 4)))
(test (parse '{- 5 2}) (sub (num 5) (num 2)))
(test (parse #t) (bool #t))


; run: src -> val
(define (run prog)
  (interp (parse prog)))


(run '{+ 3 {+ 2 1}})
(run '{- 3 {- 5 2}})

(run '{bool #t})