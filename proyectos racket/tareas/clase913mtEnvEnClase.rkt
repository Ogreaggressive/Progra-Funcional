#lang play
(print-only-errors #t)
#|
<F1WAE> ::=   <num> | <bool> | <id>
            | (+ <F1WAE> <F1WAE>)
            | (- <F1WAE> <F1WAE>)
            | (if-tf <F1WAE> <F1WAE> <F1WAE>)
            | (with <id> <F1WAE> <F1WAE>)
            | (app <id> <F1WAE>) ; aplicacion de funcion
|#

(deftype Expr
  [num n]                                 ; <num>
  [bool b]                                ; <bool>
  [add l r]                               ; (+ <F1WAE> <F1WAE>)
  [sub l r]                               ; (- <F1WAE> <F1WAE>)
  [if-tf c et ef]                         ; (if-tf <F1WAE> <F1WAE> <F1WAE>)
  [with id-name named-expr body-expr]     ; (with <id> <F1WAE> <F1WAE>)
  [id name]                               ; <id> 
  [app fname arg-expr]                    ; (app <id> <F1WAE>)
)

; <fundef> := {define {<id> <id>} <expr>}
(deftype Fundef
  (fundef fname arg body)
  )
#|
<env> ::= (mtEnv)
          | (aEnv <id> <val> <env>)
|#
(deftype Env
  (mtEnv)
  (aEnv id val env)
  )

; empty-env -> (mtEnv)
(define empty-env (mtEnv))

; extend-env:: <id> <val> <env> -> <env>
(define extend-env aEnv)
; env-lookup :: <id> <env> -> <val>
; buscar el valor de una variable dentro del ambiete
(define (env-lookup x env)
  (match env
    [(mtEnv) (error "undefined: " x)]
    [(aEnv id val tail)(if (eq? id x) val (env-lookup x tail))]
    )
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
    [(list 'if-tf c et ef) (if-tf (parse c) (parse et) (parse ef))]
    [(list 'with (list x e) b) (with x (parse e) (parse b))]
    [(list fname e) (app fname (parse e))]
    [(list 'define (list name arg) body)(fundef name arg (parse body))]
    )
  )

#|
; fun-parse: Src -> Expr
; parsea funciones, esto esta ahora por separado, pero puede integrarse al parser definido
(define (fun-parse src)
  (match src
    [(list 'define (list name arg) body)(fundef name arg (fun-parse body))]
    [else (parse src)]
    )
  )
|#



; lookup-fundef: id fundefs -> fundef o error
(define (lookup-fundef fname fundefs)
  (match fundefs
    [(list) (error "undefined function: " fname)]
    [(cons fd fds) (if (eq? (fundef-fname fd) fname)
                       fd
                       (lookup-fundef fname fds)
                       )]
    )
  )

; interp :: Expr List(fundefs) Env -> val
; interpreta una expresion
(define (interp expr fundefs env)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(id x) (env-lookup x env)]; buscar el valor de x en env
    [(add l r) (+ (interp l fundefs env) (interp r fundefs env))]
    [(sub l r) (- (interp l fundefs env) (interp r fundefs env))]
    [(if-tf c et ef) (if (interp c fundefs env)
                         (interp et fundefs env)
                         (interp ef fundefs env))]
    [(with x e b) ; {with {x e} b}
     ; 1. interp e para obtener el valor de la variable
     ; 2. agregar x y el valor de e al ambiente, extender el ambiente
     ; 3. interp b con el nuevo ambiente
     (interp b fundefs (extend-env x (interp e fundefs env) env))]
     ;(interp (subst x (parse (interp e fundefs)) b) fundefs)]
    [(app f e)
     ; 1. buscar la funcion
     (def (fundef name arg body) (lookup-fundef f fundefs))
     ; 2. evaluar el argumento
     ; 3. extender env
     ; 4. interp body con el nuevo env
     ;(interp body fundefs (extend-env arg (interp e fundefs env) env))
     (interp body fundefs (extend-env arg (interp e fundefs env) empty-env))
     ;(interp (subst arg (parse (interp e fundefs)) body) fundefs)
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
(test (run '{with {x 3} x}) 3)
(test (run '{with {x 3} {with {y 4} x}}) 3)
(test (run '{with {x 3} {+ x 4}}) 7)
(test (run '{with {x 3} {with {x 10} {+ x x}}}) 20)
(test (run '{with {x 3} {with {x x} {+ x x}}}) 6)
(test (run '{with {x 3} {with {y 2} {+ x y}}}) 5)
(test (run '{with {x 3} {+ 1 {with {y 2} {+ x y}}}}) 6)
(test (run '{with {x 3} {with {y {+ 2 x}} {+ x y}}}) 8)
(test (run '{with {x 3} {if-tf {+ x 1} {+ x 3} {+ x 9}}}) 6)

(run '{with {n 5} {f 10}} (list '{define {f x} {+ x n}}))


(test/exn (run '{f 10}) "undefined function")
(test (run '{f 10} (list '{define {f x} {+ x x}})) 20)
;Anidemos y funciona.
(test (run '{add1 {add1 {add1 10}}}(list '{define {add1 x} {+ x 1}})) 13)
(run '{foo 10}(list '{define {add1 x} {+ x 1}}
                    '{define {foo x} {+ {add1 x} {add1 x}}}))
; (x (num 10) (app 'add1 (id 'x))) Falla aqui pues no tenemos definido el caso 
(run '{with {x 2} {with {y 10} {+ {add1 x} {foo y}}}}
     (list '{define {add1 x} {+ x 1}}
           '{define {foo x} {+ {add1 x} {add1 x}}}))
