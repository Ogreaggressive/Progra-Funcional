#lang play

#|
<Src> ::=   <number?>
          | {'+ <Src> <Src>}
          | {'- <Src> <Src>}
          | {'* <Src> <Src>}
          | {'- <Src>}

<Expr> ::=   (num <number?>)
           | (add <Expr> <Expr>)
           | (sub <Expr> <Expr>)
           | {mult <Src> <Src>}
           | {change - <Src>}
|#
(deftype Expr
  [num n]
  [add l r]
  [sub l r]
  [mult l r]
  [change l]
  [multList l]
  )

; interp :: Expr -> number?
; evalua una expresion aritmetica.

(define (interp expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp l) (interp r))]
    [(sub l r) (- (interp l) (interp r))]
    [(mult l r) (* (interp l) (interp r))]
    [(change l) (* -1 (interp l))]
    [(multList l) (foldr * 1 list)]
))

; parse: Src -> Expr
; parsea codigo fuente
(define (parse src)
  (match src
    [(? number?) (num src)] 
    [(list '+ s1 s2) (add (parse s1) (parse s2))]
    [(list '- s1 s2) (sub (parse s1) (parse s2))]
    [(list '* s1 s2) (mult (parse s1) (parse s2))]
    [(list '- s1) (change (parse s1))]
     [(cons '* vals) (multList (map parse vals))]
    )
  )

; run: Src -> Expr
; corre un programa
(define (run prog)
  (interp (parse prog))
  )

(foldl + 0 '(1 2 3 4))


(test (run '{+ 3 4}) 7)
(test (run '{- 5 1}) 4)
(test (run '{- 5 {+ 2 3}}) 0)
(test (run '{- 1}) -1)
(test (run '{- {+ 2 3}}) -5)
(test (run '{* 2 4}) 8)
(test (run '{* 3 {+ 3 3}}) 18)
(test (run '{* '(1 2 3 4 5)}) 120)