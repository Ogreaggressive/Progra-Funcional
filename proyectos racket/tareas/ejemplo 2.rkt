#lang play

(cons 1 2)
(car (cons 1 2))
(cdr (cons 1 2))

;listas
empty

(cons 1 empty)
(cons 2 (cons 1 empty))
(list 1 2 3 4 (+ 4 1))
'(1 2 3 4 (+ 4 1))

(define (fact n)
  (if (zero? n) 1 (* n (fact (- n 1)))))

(fact 5)

(define l '(1 2 3 4))

(car l)
(cdr l)
(first l)
(rest l)

{define (sumL lst)
(cond
  [(empty? lst) 0]
  [else (+ (first lst) (sumL (rest lst)))]
  )}

(sumL l)
(sumL '())

;append cons
;definir una funcion (add-pair-n par n i.e. '(1 . 2) 5 -> '(6 . 7)

{define (add-pair-n par n)
  (cons (+ n (car par)) (+ n (cdr par)))
  }

(test (add-pair-n (cons 1 2) 5) (cons 6 7))


;replace 2 'a (1 2 3) -> (1 'a 3) tiene que ser recursivo
;no puedes cambiar vector, tienes que crear una lista

(define (replace valor1 valor2 lst)
  (cond
    [(empty? lst)car lst]
    [(eq? valor1 (car lst))(cons valor2 (cdr lst))]
    [else (cons (car lst) (replace valor1 valor2 (cdr lst)))]
  )
)
(test (replace 1 5 (list 3 1 2)) (list 3 5 2))
(test (replace 1 2 '()) '())

;par y lista no son mutables
;vector es mutable

(def vec (vector 1 2 3 4))
vec
(vector-set! vec 3 3)
vec




