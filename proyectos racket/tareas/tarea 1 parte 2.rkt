#lang play
;1. Implementa una función recursiva que emule el comportamiento de map:
;(define (my-map fun lista))
"ejercicio1"

#|
<my-map> ::= <func> <val><list>

<function> ::= <add> | expresion que afecte a un elemento de lista.
                (add <num> <num>)
<val> ::= (num <number>) | <list(pos)>)
|#


(define lst (list 1 2 3 4))
(define (add2 n) (+ n 2))
(define (double n) (* n 2))

(define (my_map fun lista)
  (cond
    [(empty? lista) '()]
    [else (cons (fun (car lista)) (my_map fun (cdr lista)))]
    ))

(my_map add2 lst)


(test (my_map add2 lst) '(3 4 5 6))
(test (my_map double lst) '(2 4 6 8))
(test (my_map add2 '()) '())



;2. Define una función recursiva que emule el comportamiento de foldr:
;(define (my-foldr fun initVal lst))
"ejercicio2"
#|
<my-foldr> ::= <func> <val><list>

<function> ::= <add> | expresion que afecte a un elemento de lista.
                (add <num> <list>)
<val> ::= (num <number>)
|#
(foldr cons '(4 5) '(1 2 3))

(define lst2 (list 1 2 3 4))
(define (add a b) (+ a b))
(define (res a b) (- a b))

(define (my-foldr fun initVal lst)
  (cond
    [(empty? lst) initVal]
    [else (fun (car lst) (my-foldr fun initVal (cdr lst)))]
    ))

(test (my-foldr add 0 '()) 0)
(test (my-foldr add 0 lst2) 10)
(test (my-foldr add 4 lst2) 14)
(test (my-foldr res 1 lst2) -1)

;3. Implementa la función reject utilizada de ejemplo en clase,
;pero utilizando la función filter y funciones anónimas.
#|
<reject> ::= <list> | <function>

<function> ::= (odd <number>) | (even <number>)
|#
  "ejercicio3"
(define (reject lst fun)
  (filter (lambda (x) (not (fun x))) lst))

(test (reject lst odd?) '(2 4))
(test (reject lst even?) '(1 3))
(test (reject '() even?) '())

#|4. Implementa la función Merge para listas de números, utilizando recursión,
filtros y concetenación de listas. Use el elemento del medio (len-lista)/2
como pivote. (Puedes usar las funciones take y drop ya incorporadas en Scheme)

hecho con ayuda de:
https://stackoverflow.com/questions/8725832/how-to-split-list-into-evenly-sized-chunks-in-racket-scheme
chatGPT
|#

"ejercicio4"

#|
<merge> ::=   <list><list>
<split list> ::=  <list>
<merge-sort> ::=  <list>

<list> :=   (list)
          | (cons <val> <list>)
|#


; junta listas
(define (merge list1 list2)
  (cond
    [(null? list1) list2]
    [(null? list2) list1]
    [else
     (let ((elem1 (car list1))
           (elem2 (car list2)))
       (if (<= elem1 elem2)
           (cons elem1 (merge (cdr list1) list2))
           (cons elem2 (merge list1 (cdr list2)))))]
    ))

; divide lista
(define (split-list lst)
  (let ((half (quotient (length lst) 2)))
    (cons (take lst half) (drop lst half))
    ))

; ordena lista
(define (merge-sort lst)
  (cond
    [(or (null? lst) (null? (cdr lst))) lst]
    [else
     (let* ((halves (split-list lst))
            (left (car halves))
            (right (cdr halves)))
       (merge (merge-sort left) (merge-sort right)))]
    ))

(test (merge-sort '(3 1 4 2 5)) '(1 2 3 4 5))
(test (merge-sort '()) '())
(test (merge-sort '(5 4 3 2 1)) '(1 2 3 4 5))
(test (merge-sort '(10 3 7 1 5 8)) '(1 3 5 7 8 10))

#|5. Generaliza la versión del Mergesort que funcione como la anterior pero que también acepte como
argumento una función de comparación para el ordenamiento de los elementos (define (mergesort lst fun-comp)),
donde fun-comp sea < para mayor a menor o > para menor a mayor.|#
"ejercicio5"
(define (merge-sort-both lst fun-comp) ;permite comparar con > o <
  (define (merge lista1 lista2)
    (cond
      [(null? lista1) lista2]
      [(null? lista2) lista1]
      [else
       (let ((elem1 (car lista1))
             (elem2 (car lista2)))
         (if (fun-comp elem1 elem2) ;ya no usa <= como antes sino ahora la fun que enviamos
             (cons elem1 (merge (cdr lista1) lista2))
             (cons elem2 (merge lista1 (cdr lista2)))))]
      ))

  (define (split-list lst)
    (let ((half (quotient (length lst) 2)))
      (cons (take lst half) (drop lst half))
      ))

  (define (merge-sort lst)
    (cond
      [(or (null? lst) (null? (cdr lst))) lst]
      [else
       (let* ((halves (split-list lst))
              (left (car halves))
              (right (cdr halves)))
         (merge (merge-sort left) (merge-sort right)))]
      ))

  (merge-sort lst)) ;llamamos a la funcion que llamabamos en ejercicio 4 para realizar el nuevo mergesort

(test (merge-sort-both '(3 1 4 2 5) <) '(1 2 3 4 5))
(test (merge-sort-both '() >) '())
(test (merge-sort-both '(5 4 3 2 1) >) '(5 4 3 2 1))
(test (merge-sort-both '(10 3 7 1 5 8) >) '(10 8 7 5 3 1))

#|6. Define, bajo el concepto de una función que devuelve otra función una función (more-than n)
que me permita crear comparadores a partir de un número.
(test (filter (more-than 10) '(13 2 31 45 9 10)) '(13 31 45))|#
"ejercicio6"
#|
<more-than> ::=   <comparator><list>

<comparator> ::=   "< >"
<list> :=   (list)
          | (cons <val> <list>)
|#

(define (more-than n)
  (define (comparator x)
    (> x n))
  comparator)

(test (filter (more-than 10) '(13 2 31 45 9 10)) '(13 31 45))
(test (filter (more-than 20) '(13 2 31 45 9 10)) '(31 45))
(test (filter (more-than 50) '(13 2 31 45 9 10)) '())

#|7. Usando inducción estructural para árboles binarios, define la función(max-bt bt)
que devuelve el máximo de un árbol.

hecho con ayuda de https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._max%29%29
|#
"ejercicio7"

#|
<bintree> ::=   <val>
              | (list <val> <bintree> <bintree>
|#

(deftype BinTree
  (leaf value)
  (node value left right)
)

(define (maxBT bt)
  (match bt
    [(leaf v) v]
    [(node v left right)
     (let* ((left-max (maxBT left))
            (right-max (maxBT right)))
       (max v left-max right-max))]
    ))

(define tree1 (node 4 (leaf 5) (node 3 (leaf 1) (leaf 2))))
(define tree2 (node 10 (leaf 7) (node 15 (leaf 12) (leaf 18))))
(define tree3 (node 5 (leaf 5) (node 5 (leaf 6) (leaf 5))))

(test (maxBT (leaf 1)) 1)
(test (maxBT tree1) 5)
(test (maxBT tree2) 18)
(test (maxBT tree3) 6)
