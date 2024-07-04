#lang play

#| 1.Define una funcion (rango from to) que devuelva una lista
con el rango inclusivo de los argumentos.

(rango 3 7) --> '(3 4 5 6 7)|#

(define (rango a b)
  (cond
    [(> a b) (reverse (rango b a))]
    [(eq? a b) (list a)]
    [else (cons a (rango (+ a 1) b))]
  ))

#|
 Usando esa funcion, defina una funcion (primo? num) que
devuelva si un numero es primo o no.
|#

(define (primoChris? num)
  (cond
    [(<= num 1) #f]
    [else
     (define divisores (filter (lambda (x) (= (remainder num x) 0)) (rango 2 (- num 1))))
     (= (length divisores) 0)]))

; Respuesta alternativa del ejercicio 2
(define (divisor? n)
  (λ (x)
    (if (eq? (modulo n x) 0)
        #t
        #f
    ))
  )
(define (primo? n)
  (if (eq? n 2)
      (printf "Es primo~n")
      (let* ([ran (rango 2 (- n 1))]
         [p (foldl (λ (x y) (or x y)) #f (map (divisor? n) ran))])
        (if (eq? p #t)
            (printf "No es primo~n")
            (printf "Es primo~n")))
      )
  )

(test (primo? 2)#t)
(test (primo? 17) #t)
(test (primo? 31) #t)
(test (primo? 3) #t)
(test (primo? 16) #f)
(test (primo? 98) #f)

#|
Define una funcion (xx lst) que recibe una lista de
booleanos y devuelve si #t si todos son booleanos.
|#

(define (trueList list)
  (foldr (λ (val1 val2) (and val1 val2)) #t list

   )
  )


(test (trueList '(#t)) #t)
(test (trueList '(#t #f #t)) #f)
(test (trueList '()) #t)
(test (trueList '(#f)) #f)


#|

|#

(define (yyyy lst fun)
   (foldr (lambda (a b) (cons (fun a) b)) '() lst))

(yyyy '(1 2 3) (lambda (x) (+ x 1)))

(yyyy '(4 5 6) (lambda (x) (* x 3)))



#|

Utilizando la versión del lenguaje AE visto la clase pasada, modifique la estructura para que los programas ejecutados
 no sean prefijos ('{+ 1 2}) sino infijos ('{1 + 2}).

|#

(deftype Expr
  [num n]
  [add l r]
  [sub l r])


; calc :: Expr -> number?
; evalua una expresion aritmetica.

(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
))

; parse: Src -> Expr
(define (parse src)
  (match src
    [(? number?) (num src)] ; si pasa el filtro, me interesa
    [(list s1 '+ s2) (add (parse s1) (parse s2))]
    [(list '- s1 s2) (sub (parse s1) (parse s2))]
    )
  )

(define (run prog)
  (calc (parse prog))
  )

(run '{ 1 + 2})