#lang play
#|
<list> ::= (list)
           !(cons <val><list>)

<val> ::= cualquier valor

(define (f lst)
   (match lst
     [(list) ...)
     [(cons head tail) ... head ... (contains? tail) ... tail)
)

|#

; contains? :: lst n --> boolean
;funcion que vevuelve si un elemento se encuentra

(define (contains? lst n)
  (match lst
    [(list) #f]
    [(cons head tail) (if (eq? head n)
                          #t
                          (contains? tail n))]
    ))


(test (contains? '(1 2 3 4) 5) #f)
(test (contains? '(1 2 3 4) 2) #t)
(test (contains? '() 4) #f)

#|
<bintree> := <val>
             | (node <val> left right)

(define (f bt)
   (match bt
      [(leaf v) ...]
      [(node v l r) ... v ... (f l) ... (f r)...]
))
|#

(deftype Bintree
  (leaf v)
  (node v l r)
  )

;dcontainsBT? :: BinTree n -> boolean
;funcion que busca un n en un bt
(define (containsBT? bt n)
  (match bt
    [(leaf v) (if (eq? v n) #t #f)]
    [(node v l r) (or (eq? v n)(contains BT> l n)(contains BT> r n))]
    ))

(test (containsBT? ()))
