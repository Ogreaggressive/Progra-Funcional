#lang play
#|
<expr> ::=   (num <number?>)
           | (bool <boolean?>)
           | (add <expr> <expr>)
           | (sub <expr> <expr>)
<WAE> ::= <num> | <bool>
       | (+ <AE> <AE>)
       | (- <AE> <AE>)
       | (gt n1 n2) greater than
       | (lt n1 n2) less than
       | (if-tf <AE> <AE> <AE>)
                cond if-true if-false
       | (with id-name named-expr body-expr)
       | (app <id> <wae>
|#
; (foo 4) -> (app 'foo (num 4))
;                  fun-name arg
(deftype Expr
  [num n]
  [bool b]
  [add l r]
  [sub l r]
  [gt l r]
  [lt l r]
  [if-tf c et ef]
  [with x ne b]
  [id name]
  [app fname arg]
  )

;lookup-fundef: <id> <list[fundefs]>
(define (lookup-fundef fname fundefs)
  (match fundefs
    
    )
  )



;                      name name-arg body
; <fundef> := (define (<id> <id>) <expr>)
(deftype Fundef
  (fundef fname arg body)
  )
; subst: id <WAE> <WAE> -> <WAE>
; subtituye todas las apariciones de id en el cuerpo por el valor

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



; interp : expr -> val
(define (interp expr fundefs)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(id x) (error "unidentified:" x)]
    [(add l r) (+ (interp l fundefs) (interp r fundefs))]
    [(sub l r) (- (interp l fundefs) (interp r fundefs))]
    [(gt l r) (> interp l fundefs interp r fundefs)]
    [(lt l r) (< interp l fundefs interp r fundefs)]
    [(if-tf c et ef) (if (interp c fundefs) (interp et fundefs) (interp ef fundefs))]
    [(with x ne b)
     (interp(subst x (parse (interp ne fundefs))b) fundefs)]
    [(app fname arg)
     ;1. busca la funcion fname en fundefs
     (def (fundef name arg-name body) (lookup-fundef fname fundefs))
     ;2 interp arg
     ;3 subst arg en body
     ;4 interp
     (interp (subst arg-name parse (interp arg fundefs)body) fundefs)
     ]
    ))

#|

<Src> ::= <number?> | <boolean?> 
         | {'+ <Src> <Src>}
         | {'- <Src> <Src>}

|#

; parse : src -> expr
(define (parse src)
  (match src 
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(? symbol?) (id src)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '> l r) (sub (parse l) (parse r))]
    [(list '< l r) (sub (parse l) (parse r))]
    [(list  'if-tf c et ef) (if-tf (parse c) (parse et) (parse ef))]
    [(list 'with (list id ne) body)
     (with id (parse ne) (parse body))]
    [(list fname arg) (app fname (parse arg))]
    )
  )

; run: src -> val
(define (run prog [fundefs '()])
   (interp (parse prog) fundefs))
 


(run '{+ 3 {+ 2 1}})
(run #t)

(test (run '{if-tf #t {+ 1 1} {- 1 1}}) 2)
(test (run '{if-tf #f {+ 1 1} {- 1 1}}) 0)
(test (run '{if-tf {+ 2 3} #t #f}) #t)
(test (run '{if-tf {> 24 52} #f #t}) #f)
(test (run '{if-tf {> 52 24} #t #f}) #t)
(test (run '{if-tf {< 24 52} #t #f}) #t)
(test (run '{if-tf {< 52 24} #f #t}) #f)


(test (run '{with {x 3} 2}) 2)
(test (run '{with {x 3} x}) 3)
(test (run '{with {x 3} {with {y 4} x}}) 3)
(test (run '{with {x 3} {+ x 4}}) 7)
(test (run '{with {x 3} {with {x 10} {+ x x}}}) 20)
(test (run '{with {x 3} {with {x x} {+ x x}}}) 6)
(test (run '{with {x 3} {with {y 2} {+ x y}}}) 5)
(test (run '{with {x 3} {+ 1 {with {y 2} {+ x y}}}}) 6)
(test (run '{with {x 3} {with {y {+ 2 x}} {+ x y}}}) 8)






#|
1. * infinita
Modifica el lenguaje para que soporte multiplicación infinita y actualize el type checking (2pts)
(test (run '{* 1 1 1 1}) 1)
(test/exn (run '{* 1 #t 1 1}) "error: incorrect type")
|#