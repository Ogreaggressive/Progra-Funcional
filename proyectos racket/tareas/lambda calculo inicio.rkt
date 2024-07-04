#lang play

#|
lambda = ctrl \ --> λ
lambda = funcion sin identificador.
λ calculo

<expr> ::=   x     --> variable
           | λx.e  --> funcion
           | e.e   --> aplicacion
(define (f X) (e))
(define f λx (e)
|#

(define lst (list 1 2 3 4))
(define (add2 n) (+ n 2))
;...(addL list)
(map add2 lst)

(define lstString (list "earth" "mars" "neptune" "jupiter"))
(map string-length lstString)
"fold"
(foldl cons '() '(1 2 3))
(foldr cons '(4 5) '(1 2 3))


(define (reject lst fun) ;reject when true (reject number if isEven())
  (cond [(empty? lst) '()]
        [(if (fun (car lst))
            (reject (cdr lst) fun)
            (cons (car lst) (reject (cdr lst) fun)))]
        ))
"reject"
(reject '(1 2 3) even?)
(reject '(1 2 3) odd?)


(define (3? n) (if (eq? 3 n ) #t #f))
(reject '(1 2 3) 3?) ;no recomendable

(reject '(1 2 3) (lambda (x) (eq? x 3)))

;var => {}
(define (add2.1 n) (+ n 2))
;add 124
;add 78
;add 2145
(map add2.1 lst)
;addN :: n(number? -> <procedure>
;generador de funciones que devuelve una funcion que suma x a un valor n
(define (addN n)
  (λ (x) (+ x n)))
(addN 78)

(map (addN 78) lst)
(map (addN 124) lst)




