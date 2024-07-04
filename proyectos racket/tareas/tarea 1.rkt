#lang play

;función que cambia aleatoriamente el valor de uno de los elementos de un vector dado.
(make-vector 0 0)
(make-vector 3 2)
(vector 1 2 3 4)

;documentacion:
;https://docs.racket-lang.org/reference/vectors.html#%28def._%28%28quote._~23~25kernel%29._vector-set%21%29%29
;https://docs.racket-lang.org/reference/generic-numbers.html#%28part._.Random_.Numbers%29
(define (randomChange vector)
  (cond
    [(empty? vector) (make-vector 0 0)]
    [else(vector-set! vector (random (vector-length vector)) (random 10)) vector]
    ))

(define vectorPrueba (vector 1 2 3 4 5))
vectorPrueba
(randomChange vectorPrueba)

;Define una función de potencia que eleve un número a un exponente, escribe tests para probar la funcionalidad.

(define (potencia base exponente)
  (if (= exponente 0) 1
      (* base (potencia base (- exponente 1)))
      ))

; Tests
(test (potencia 2 0) 1)
(test (potencia 2 1) 2)
(test (potencia 2 3) 8)
(test (potencia 2 10) 1024)

;Define una función Fibonacci que devuelva el n-ésimo Fibonacci, escribe tests para probar la funcionalidad.

(define (fibonacci n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (fibonacci (- n 1)) (fibonacci (- n 2)))]))

; Tests
(test (fibonacci 0) 0)
(test (fibonacci 1) 1)
(test (fibonacci 5) 5)
(test (fibonacci 7) 13)

;Implementa la función rember del punto 4 de la parte anterior.
(define (rember a lat)
  (cond
    [(empty? lat) '()]
    [(equal? (car lat) a) (cdr lat)]
    [else (cons (car lat) (rember a (cdr lat)))]
    ))

(define latOriginal '("lamb" "chops" "and" "mint" "jelly")) ;no funciona si no creo la lista como strings aunque car me deje ver car = lamb
latOriginal
(car latOriginal)
(rember "mint" latOriginal)


;Implementa la función insertR del punto 5 de la parte anterior.
(define (insertR new old lat)
  (cond
    [(empty? lat) '()]
    [(equal? (car lat) old) (cons (car lat) (cons new (cdr lat)))]
    [else (cons (car lat) (insertR new old (cdr lat)))]
    ))


(define lista-original '("a" "b" "c" "d" "f" "g" "d" "h"))
lista-original
(define resultado-final (insertR "e" "d" lista-original))
resultado-final
