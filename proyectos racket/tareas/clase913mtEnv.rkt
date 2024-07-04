#lang play
(print-only-errors #t)
#|
<WAE> ::=    <num> | <bool>
          | (+ <WAE> <WAE>)
          | (- <WAE> <WAE>)
          | (if-tf <WAE> <WAE> <WAE>)
          | <id>
          | (with <id> <WAE> <WAE>)
          | (app <id> <WAE>)
|#
; {foo 4} -> {app 'foo      (num 4)}
;                  fun-name arg
(deftype Expr
  [num n]
  [bool b]
  [add l r]
  [sub l r]
  [mult lst]
  [equal x n]
  [if-tf c et ef]
  [with id-name named-expr body-expr]
  [id name]
  [app fname arg]
  )
;                      name name-arg body
; <fundef> := {define {<id> <id>} <expr>}
(deftype Fundef
  (fundef fname arg body)
  )

#| <env> ::= (mtEnv)
             | (<id> <val> <env>)
|#
(deftype Env
  (mtEnv)
  (aEnv id val env)
  )

  ;empty-env --> (mtEnv)
(define empty-env (mtEnv))

  ;extend-env:: <id> <val> <env> --> <env>
(define extend-env aEnv)

  ;env-lookup :: <id> <env> --> <val>
;buscar el valor de una variable dentro del ambiente
(define (env-lookup x env)
  (match env
    [(mtEnv) (error "undefined: " x)]
    [(aEnv id val tail) (if(eq? id x)
                          val
                          (env-lookup x tail)
                          )]
    )
  )


; parse: Src -> Expr
; parsea codigo fuente
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(? symbol?) (id src)]
    [(list '= s1 s2) (equal (parse s1)(parse s2))]
    [(list '+ s1 s2) (add (parse s1) (parse s2))]
    [(list '- s1 s2) (sub (parse s1) (parse s2))]
    [(cons '* vals) (mult (map parse vals))]
    [(list 'if-tf c et ef) (if-tf (parse c) (parse et) (parse ef))]
    [(list 'with (list x e) b) (with x (parse e) (parse b))]
    [(list fname arg) (app fname (parse arg))]
    [(list 'define (list name arg) body)(fundef name arg (parse body))]
    )
  )

; lookup-fundef: <id> <list[Fundefs]> -> <fundef>
(define (lookup-fundef fname fundefs)
  (match fundefs
    [(list) (error "undefined: " fname)]
    [(cons head tail) (cond [(eq? (fundef-fname head) fname) head]
                           [(lookup-fundef fname tail)]
                           ) ]
    )
  )

;fun-parse: src -> expr
(define (fun-parse src)
  (match src
  [(list 'define (list fname arg-name) body) (fundef fname arg-name (parse body))
                                             ]
  ))

; interp :: Expr List(fundefs) Env -> val

(define (interp expr fundefs env)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(id x) (env-lookup x env)] ;buscar el valor de x en env
    [(equal x n)(if (= (interp x fundefs env) (interp n fundefs env)) #t #f)]
    [(add l r) (+ (interp l fundefs env) (interp r fundefs env))]
    [(sub l r) (- (interp l fundefs env) (interp r fundefs env))]
    [(mult lst) (foldr * 1 (map (λ (expr) (interp expr fundefs env)) lst))]
    [(if-tf c et ef) (if (interp c fundefs env)
                         (interp et fundefs env)
                         (interp ef fundefs env))]
    [(with x e b) ; {with {x e} b}
     ;1. interp e para obtener el valor de la variable
     ;2. agregar x y el valor de e al ambiente, extender el ambiente
     ;3. interp x con el nuevo ambiente
     (interp b fundefs (extend-env x (interp e fundefs env) env))]
    [(app fname arg)
     ;1. Buscar la funcion fname en fundefs
     (def (fundef name arg-name body) (lookup-fundef fname fundefs))
     ;2. interp arg
     ;3. extender env
     ;4. interp body con el nuevo env
     (interp body fundefs (extend-env arg-name (interp arg-name fundefs env) env))
     ]
))


; run: Src list<fundef>? -> Expr
; corre un programa
(define (run prog [fundefs '()])
  (interp (parse prog) (map parse fundefs) empty-env)
  )

(test (run '{+ 3 4}) 7)
(test (run '{- 5 1}) 4)

(test (run '{with {x 3} 2}) 2)
(test (run '{with {x 3} x})3)
(test (run '{with {x 3} {with {y 4} x}}) 3)
(test (run '{with {x 3} {+ x 4}}) 7)
(test (run '{with {x 3} {with {x 10} {+ x x}}}) 20)
(test (run '{with {x 3} {with {x x} {+ x x}}}) 6)
(test (run '{with {x 3} {with {y 2} {+ x y}}}) 5)
(test (run '{with {x 3} {+ 1 {with {y 2} {+ x y}}}}) 6)
(test (run '{with {x 3} {with {y {+ 2 x}} {+ x y}}}) 8)
(test (run '{with {x 3} {if-tf {+ x 1} {+ x 3} {+ x 9}}}) 6)
;(test (run '{foo 5} (list ('define 'foo 'x (parse '{+ x x})))) 10)
;(test (run '{foo 5} (list ('define 'foo 'x (parse '{+ x x})))) 10)

(test (run '{foo 10} (list {list 'define '(add1 x) '(+ x 1)} {list 'define '(foo x) '(+ (add1 x) (add1 x)) })) 22)

 (run '{fact 10} (list {list
                            'define '(fact x)
                            '(if-tf (= x 0) 1 (* x (fact (- x 1))))}
                           {list 'define '(foo x) '(fact 5) }))


;(run '(f 10) (list '(define (f x) (f x))))
(define (copy expr)
  (match expr
    [(num n) (num n)]
    [(bool b) (bool b)]
    [(id x) (id x)]
    [(add l r) (add (copy l) (copy r))]
    [(sub l r) (sub (copy l) (copy r))]
    [(if-tf c et ef) (if (copy c)
                         (copy et)
                         (copy ef))]
    [(with x e b) (with  x (copy e) (copy b))]
    [(app f e) (app f (copy e))
     ]
    )
  )

#|
(test (count-fun (parse 2)) 0)
(test (count-fun (parse '{+ 1 5 })) 0)
(test (count-fun (parse '{{foo 3} {bar 5}})) 2)
(test (count-fun (parse '{if-tf #t {foo 3} {bar 5}})) 2)
|#



