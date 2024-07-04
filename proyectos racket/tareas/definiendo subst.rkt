#lang play
#|
<WAE> ::=    <num> | <bool> |<id>
          | (+ <WAE> <WAE>)
          | (- <WAE> <WAE>)
          | (with <id> <WAE> <WAE>
          
|#
(deftype Expr
  [num n]
  [bool b]
  [add l r]
  [sub l r]
  [id sym]
  [with x ne b]
  )

; parse: Src -> Expr
; parsea codigo fuente
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(? symbol?) (id src)]
    [(list '+ s1 s2) (add (parse s1) (parse s2))]
    [(list '- s1 s2) (sub (parse s1) (parse s2))]
    [(list 'with (list id ne) body)
     (with id (parse ne) (parse body))]
    )
  )

; subst: id <WAE> <WAE> -> <WAE>
; subtituye todas las apariciones de id en el cuerpo por el valor

;subst :: id expr expr -> expr
;substituir x por v en e
(define (subst x v e)
  (match e
    [(num n) e]
    [(id sym) (if (eq? x sym) v e)]
    [(add l r) (add (subst x v l) (subst x v r))]
    [(sub l r) (sub (subst x v l) (subst x v r))]
    [(with id ne b)
     (with id
           (subst x v ne)
           (if (eq? x id)
               b
               (subst x v b)
               ))]
   )
 )

(define (interp expr)
  (match expr
    [(num n) n]
    [(id x) (error "undefined: " x)]
    [(add l r) (+ (interp l) (interp r))]
    [(sub l r) (- (interp l) (interp r))]
    [(with x e b) ; {with {x e} b}
     (interp (subst x (parse (interp e)) b))]
))


; run: Src -> Expr
; corre un programa
(define (run prog)
  (interp (parse prog))
  )

(let ([x 3])
  (let ([x x])
    (+ x x)
    )
  )
(test (run '{with {x 3} 2}) 2)
(test (run '{with {x 3} x}) 3)
(test (run '{with {x 3} {with {y 4} x}}) 3)
(test (run '{with {x 3} {+ x 4}}) 7)
(test (run '{with {x 3} {with {x 10} {+ x x}}}) 20)
(test (run '{with {x 3} {with {x x} {+ x x}}}) 6)
(test (run '{with {x 3} {with {y 2} {+ x y}}}) 5)
(test (run '{with {x 3} {+ 1 {with {y 2} {+ x y}}}}) 6)
(test (run '{with {x 3} {with {y {+ 2 x}} {+ x y}}}) 8)
(test (run '{with {x 2} {with {y 3} {with {z 1} {+ x {+ y z}}}}}) 6)

(test (run '{+ 3 4}) 7)
(test (run '{- 5 1}) 4)