#lang play
(print-only-errors #t) ; Para ver solo los errores.
;Práctica 3 - FAE

;Nombre: Nicolas Cari Rodriguez

;Asignatura: Programación Funcional


#|
<expr> ::=   <num> | <bool> | <id>
            | (+ <expr> <expr>)
            | (with <id> <expr> <expr>)
            | (app <id> <expr>) 
|#

#|
<FAE> ::=   <num> | <bool> | <id>
            | (prim op-name <FAE> ... <FAE>)
            | (if-tf <FAE> <FAE> <FAE>)
            | (with <id> <FAE> <FAE>)
            | (app <FAE> <FAE>) ; puedo aplicar una funcion a otra funcion / puedo usar una funcion como argumento. 
            | (fun <id> <FAE>) ; fun(que es una lambda) nombre-arg body
            | (constantFolding <
|#

(define primitives
  (list
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)
   (cons '> >)
   (cons '>= >=)
   (cons '< <)
   (cons '<= <=)
   (cons '== equal?)
   (cons '!=  (λ (val1 val2) (if(equal? val1 val2) #f #t)))
   (cons 'and (λ (x y) (and x y)))
   (cons 'or (λ (x y) (or x y)))
   (cons 'concat string-append)
   ))

; (apply (cdr (assq '+ primitives)) '(1 2 3 4))

(deftype Expr
  [num n]                                 ; <num>
  [bool b]                                ; <bool>
  [str s]
  [if-tf c et ef]                         ; (if-tf <FAE> <FAE> <FAE>)
  [with id-name named-expr body-expr]     ; (with <id> <FAE> <FAE>)
  [id name]                               ; <id> 
  [app fname arg-expr]                    ; (app <FAE> <FAE>) ; ahora podemos aplicar una funcion a otra
  [fun arg body]                          ; (fun <id> <FAE>) ; mantenemos el <id> como el nombre del argumento
  [prim name args]
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
    [(? string?) (str src)]
    [(? symbol?) (id src)]
    [(list 'if-tf c et ef) (if-tf (parse c) (parse et) (parse ef))]
    [(list 'with (list x e) b) (with x (parse e) (parse b))]
    [(list arg e) (app (parse arg) (parse e))]; 2. Subir de nivel nuestras funciones
    [(list 'fun (list arg) body) (fun arg (parse body))] ; 1. Agregar el caso del fun
    [(cons prim-name args) (prim prim-name (map parse args))]
    )
  )

(deftype Val
  (valV v) ; numero, booleano, string, byte, etc.
  (closureV arg body env) ; closure = fun + env

  )


; interp :: Expr  Env -> Val
; interpreta una expresion
(define (interp expr env)
  (match expr
    [(num n) (valV n)]
    [(bool b) (valV b)]
    [(str s) (valV s)]
    [(id x) (env-lookup x env)]; buscar el valor de x en env
    [(prim prim-name args) (prim-ops prim-name (map (λ (x) (interp x env)) args))]
    [(if-tf c et ef) (if (interp c env)
                         (interp et env)
                         (interp ef env))]
    [(with x e b) (interp b (extend-env x (interp e env) env))] ; Si asociamos una funcion a una variable, la funcion entra al env
    [(fun arg body) (closureV arg body env)] ; Por ahora, devolvemos la misma expresion que nos llego
    [(app f e)
     (def (closureV arg body fenv) (interp f env)) ; Esto permite encontrar (fun 'x (add (id 'x) (id 'x))) por ejemplo y tomar arg y body
    
     (interp body (extend-env arg (interp e env) fenv)) ; parece que no funciona ni con estatico ni dinamico
     ]
    ;[(and l r) (valV (and (interp l env) (interp r env)))]
    ;[(or l r) (valV (or (interp l env) (interp r env)))]
    ;[(str s) (valV s)]
    ;[(concat s1 s2) (valV (string-append (interp s1 env) (interp s2 env)))]
))

; prim-ops: op-name list[Val] -> Val
(define (prim-ops op-name args)
  (let ([vals (map (λ (x) (valV-v x)) args)])
    (valV (apply (cdr (assq op-name primitives)) vals))
    )
  )

; run: Src -> Src
; corre un programa
(define (run prog)
  (let ([res (interp (parse prog) empty-env)])
    (match res
      [(valV v) v]
      [(closureV arg body env) res])
    )
  )

(test (run '{+ 3 4}) 7)
(test (run '{- 5 1}) 4)
(test (run '{+ 1 2 3 4}) 10)

(test (run '{with {x 3} 2}) 2)
(test (run '{with {x 3} x}) 3)
(test (run '{with {x 3} {with {y 4} x}}) 3)
(test (run '{with {x 3} {+ x 4}}) 7)
(test (run '{with {x 3} {+ x x x x}}) 12)
(test (run '{with {x 3} {with {x 10} {+ x x}}}) 20)
(test (run '{with {x 3} {with {x x} {+ x x}}}) 6)
(test (run '{with {x 3} {with {y 2} {+ x y}}}) 5)
(test (run '{with {x 3} {+ 1 {with {y 2} {+ x y}}}}) 6)
(test (run '{with {x 3} {with {y {+ 2 x}} {+ x y}}}) 8)
(test (run '{with {x 3} {if-tf {+ x 1} {+ x 3} {+ x 9}}}) 6)


; Adaptando las pruebas previas
;(test/exn (run '{f 10}) "undefined function") - el error partia de fundef-lookup
(test/exn (run '{f 10}) "undefined")

;(test (run '{f 10} (list '{define {f x} {+ x x}})) 20)
; 1. Asociar la funcion a un identificador
(test (run '{with {f {fun {x} {+ x x}}}{f 10}}) 20)
; 2. Usar la funcion directamente, como un lambda
(test (run '{{fun {x} {+ x x}} 10}) 20)

;(test (run '{add1 {add1 {add1 10}}}(list '{define {add1 x} {+ x 1}})) 13)
(test (run '{with {add1 {fun {x} {+ x 1}}}{add1 {add1 {add1 10}}}}) 13)


#|
(run '{foo 10}(list '{define {add1 x} {+ x 1}}
                    '{define {foo x} {+ {add1 x} {add1 x}}}))


En este caso , las funciones ya han dejado de ser visibles para todos por lo que ejecutar esto
resultara en un error. sin embargo, se puede enviar la funcion como argumento

; Prueba fallida
(test (run '{with {add1 {fun {x} {+ x 1}}}
                  {with {foo {fun {x} {+ {add1 x} {add1 x}}}}
                        {foo 10}}}) 22)

(test (run '{with {add1 {fun {x} {+ x 1}}}
                  {with {foo {fun {f} {+ {f 10} {f 10}}}}
                        {foo add1}}}) 22)


; Pruebas para casos basicos
(test (run '{{fun {x}{+ x 1}} {+ 2 3}}) 6)
(test (run '{with {apply10 {fun {f} {f 10}}}
                  {with {add1 {fun {x} {+ x 1}}}
                        {apply10 add1}}}) 11)
|#

; Prueba fallida
(test (run '{with {add1 {fun {x} {+ x 1}}}
                  {with {foo {fun {x} {+ {add1 x} {add1 x}}}}
                        {foo 10}}}) 22)

(test (run '{with {add1 {fun {x} {+ x 1}}}
                  {with {foo {fun {f} {+ {f 10} {f 10}}}}
                        {foo add1}}}) 22)


; Pruebas para casos basicos
(test (run '{{fun {x}{+ x 1}} {+ 2 3}}) 6)
(test (run '{with {apply10 {fun {f} {f 10}}}
                  {with {add1 {fun {x} {+ x 1}}}
                        {apply10 add1}}}) 11)

#|
Sin embargo, la currificacion falla en la implementacion actual.


|#

(test (run '{with {addN {fun {n}
                       {fun {x} {+ x n n n n}}}}
            {{addN 10} 20}}) 60)



;1.Extiende el lenguaje FAE para soportar las operaciones: + - * / < <= >= > == != Escribe pruebas para la extensión. (1pt)

'multAndDivTestCases
; Addition Test Cases
(test (run '{+ 3 4}) 7)
(test (run '{+ 0 0}) 0)
(test (run '{+ -1 1}) 0)
(test (run '{+ -1 -1}) -2)

; Subtraction Test Cases
(test (run '{- 5 1}) 4)
(test (run '{- 0 0}) 0)
(test (run '{- -5 -3}) -2)
(test (run '{- -5 3}) -8)

; Multiplication Test Cases
(test (run '{* 2 3}) 6)
(test (run '{* 0 5}) 0)
(test (run '{* -2 -4}) 8)
(test (run '{* -2 4}) -8)

; Division Test Cases
(test (run '{/ 10 2}) 5)
; NO HACE FALTA CONTROL DE ERRORES AL DIVIDIR ENTRE 0
(test (run '{/ -10 2}) -5)
(test (run '{/ -10 -2}) 5)

'infiniteArguments

; Addition with Infinite Arguments
(test (run '{+ 1 2 3 4 5 6 7 8 9 10}) 55)
(test (run '{+ 0 0 0 0 0 0 0 0 0 0}) 0)
(test (run '{+ 2 4 6 8 10 12 14 16 18 20}) 110)

; Subtraction with Infinite Arguments
(test (run '{- 10 2 2 2 2 2 2 2}) -4)
(test (run '{- 100 50 25 10 5 2 1}) 7)
(test (run '{- 10 1 1 1 1 1 1 1 1 1}) 1)

; Multiplication with Infinite Arguments
(test (run '{* 1 2 3 4 5 6 7 8 9 10}) 3628800)
(test (run '{* 2 2 2 2 2 2 2 2}) 256)
(test (run '{* 3 3 3 3 3 3 3 3}) 6561)

; Division with Infinite Arguments
(test (run '{/ 100 2 2 2 2 2 2 2}) 25/32)
(test (run '{/ 64 4 4 4 4}) 1/4)
(test (run '{/ 100 5 5 5 5}) 4/25)

'EqualMoreandLessCases

; Equal To Test Cases
(test (run '{== 5 5}) #t)
(test (run '{== 2 5}) #f)
(test (run '{== #t #f}) #f)

; Not Equal To Test Cases
(test (run '{!= 5 5}) #f)
(test (run '{!= 2 5}) #t)
(test (run '{!= #t #f}) #t)

; Greater Than Test Cases
(test (run '{> 5 2}) #t)
(test (run '{> 2 5}) #f)
(test (run '{> 2 2}) #f)

; Greater Than or Equal To Test Cases
(test (run '{>= 5 2}) #t)
(test (run '{>= 2 5}) #f)
(test (run '{>= 2 2}) #t)


; Less Than Test Cases
(test (run '{< 5 2}) #f)
(test (run '{< 2 5}) #t)
(test (run '{< 2 2}) #f)

; Less Than or Equal To Test Cases
(test (run '{<= 5 2}) #f)
(test (run '{<= 2 5}) #t)
(test (run '{<= 2 2}) #t)


;2.Extiende el lenguaje FAE con las operaciones lógicas and y or. Además, agrega soporte para strings y concatenación de cadenas. Escribe pruebas (1pt)

;las pruebas hice que ChatGPT haga directamente para ver si funcionaa

'orYand

; Pruebas para la operación lógica "and"
(test (run '{and #t #t}) #t) ; Valor verdadero y valor verdadero = Valor verdadero
(test (run '{and #f #t}) #f) ; Valor falso y valor verdadero = Valor falso
(test (run '{and #f #f}) #f) ; Valor falso y valor falso = Valor falso
(test (run '{and #t #f}) #f) ; Valor verdadero y valor falso = Valor falso

(test (run '{and {> 5 3} {<= 4 9}}) #t) ; 5 > 3 y 4 <= 9 = Valor verdadero
(test (run '{and {== 5 5} {!= 48 48}}) #f) ; 5 == 5 y 48 != 48 = Valor falso

; Pruebas para la operación lógica "or"
(test (run '{or #t #f}) #t) ; Valor verdadero o valor falso = Valor verdadero
(test (run '{or #f #f}) #f) ; Valor falso o valor falso = Valor falso
(test (run '{or {> 5 3} {<= 4 9}}) #t) ; 5 > 3 o 4 <= 9 = Valor verdadero
(test (run '{or {== 5 5} {!= 48 48}}) #t) ; 5 == 5 o 48 != 48 = Valor verdadero

; Pruebas para la concatenación de cadenas
(test (run '(concat "Hola, " "mundo!")) "Hola, mundo!") ; Concatenación de dos cadenas
(test (run '(concat "Este " "es " "un " "test.")) "Este es un test.") ; Concatenación de múltiples cadenas
(test (run '(concat "123" " " "456")) "123 456") ; Concatenación de números en formato de cadena


;3.Constant Folding - Esta es una optimización utilizada en compiladores para eliminar el cálculo innecesario de
;expresiones constantes y reemplazarlas directamente con su valor
;En general, si existe una operación que no contiene variables,se espera simplificarla.
;Las siguientes pruebas deben pasar (2pt): 


(define (contains-ids expr)
  (match expr
    [(cons (id x) tail) #f]
    [(cons x tail) (contains-ids tail)]
    [(list) #t]
    )
  )

#|
(define (interp expr env)
  (match expr
    [(num n) (valV n)]
    [(bool b) (valV b)]
    [(str s) (valV s)]
    [(id x) (env-lookup x env)]; buscar el valor de x en env
    [(prim prim-name args) (prim-ops prim-name (map (λ (x) (interp x env)) args))]
    [(if-tf c et ef) (if (interp c env)
                         (interp et env)
                         (interp ef env))]
    [(with x e b) (interp b (extend-env x (interp e env) env))] ; Si asociamos una funcion a una variable, la funcion entra al env
    [(fun arg body) (closureV arg body env)] ; Por ahora, devolvemos la misma expresion que nos llego
    [(app f e)
     (def (closureV arg body fenv) (interp f env)) ; Esto permite encontrar (fun 'x (add (id 'x) (id 'x))) por ejemplo y tomar arg y body
    
     (interp body (extend-env arg (interp e env) fenv)) ; parece que no funciona ni con estatico ni dinamico
|#

;copiamos interp y cambiamos ahi
;devolvemos lo mismo que conseguimos en todo menos prim
(define (constant-folding expr)
  (match expr
    [(num n) (num n)]
    [(bool b) (bool b)]
    [(str s) (str s)]
    [(id x) (id x)]
    ;verifica si la expresión contiene identificadores o si son valores constantes.
    ;si son constantes entonces simplifica.
    [(prim prim-name args) (if (contains-ids args) 
                                               (parse (valV-v (interp (prim prim-name args) mtEnv))) ;si es constante interpretamos, luego evaluamos y parseamos
                                               (prim prim-name (map constant-folding args)) ;hacemos constant folding en cada arg
                                               )]
    [(if-tf c et ef) (if-tf (constant-folding c)
                         (constant-folding et)
                         (constant-folding ef))]
    [(with x e b) (with x  (constant-folding e) (constant-folding b))]
    [(fun arg body) (fun  arg (constant-folding body))] 
    [(app f e)
     (app (constant-folding f) (constant-folding e))
     
     ]
)
  )
'ejercicio3
(test (constant-folding (parse '{with {x {+ 1 2 {* 3 4}}} {+ 1 x}}))
      (parse '{with {x 15} {+ 1 x}}))
(test (constant-folding (parse '{with {x {+ y 2 {* 3 4}}} {+ 1 x}}))
      (parse '{with {x {+ y 2 12}} {+ 1 x}}))
(test (constant-folding (parse '{if-tf {< x 1} {+ 3 3} {+ 5 9}}))
      (parse '{if-tf {< x 1} 6 14}))
(test (constant-folding (parse'{{fun {x} {+ x {* 2 4}}} {+ 5 5}}))
      (parse '{{fun {x} {+ x 8}} 10}))



;4.  Constant Propagation. Otra optimizacion en compiladores es constant propagation. Cuando se reconoce que
;un identificador es constante, entonces se reemplazan sus ocurrencias inmediatamente en lugar
;de mantener la substitución hasta el final. Las siguientes pruebas deben pasar (2pt):

;hecho con ayuda de Christian Rivero

;busca un valor constante asociado a una variable en un entorno dado. 
(define (find-constant x env)
  (match env
    [(mtEnv) (id x)] ; Si no se encuentra, devolver la variable original
    [(aEnv id val tail)
     (if (eq? id x)
         val 
         (find-constant x tail))]))


#|
(define (interp expr env)
  (match expr
    [(num n) (valV n)]
    [(bool b) (valV b)]
    [(str s) (valV s)]
    [(id x) (env-lookup x env)]; buscar el valor de x en env
    [(prim prim-name args) (prim-ops prim-name (map (λ (x) (interp x env)) args))]
    [(if-tf c et ef) (if (interp c env)
                         (interp et env)
                         (interp ef env))]
    [(with x e b) (interp b (extend-env x (interp e env) env))] ; Si asociamos una funcion a una variable, la funcion entra al env
    [(fun arg body) (closureV arg body env)] ; Por ahora, devolvemos la misma expresion que nos llego
    [(app f e)
     (def (closureV arg body fenv) (interp f env)) ; Esto permite encontrar (fun 'x (add (id 'x) (id 'x))) por ejemplo y tomar arg y body
    
     (interp body (extend-env arg (interp e env) fenv)) ; parece que no funciona ni con estatico ni dinamico
|#
;cambiamos interp por constant-propagation como en 3
(define (constant-propagation expr [env (mtEnv)])
  (match expr
    [(num n) (num n)]
    [(bool b) (bool b)]
    [(str s) (str s)]
    [(id x) (find-constant x env)];en id X usamos find-constant para encontrar la variable en env
    [(prim prim-name args) (prim prim-name (map (λ (arg) (constant-propagation arg env)) args))]
    [(if-tf c et ef) (if-tf (constant-propagation c env)
                         (constant-propagation et env)
                         (constant-propagation ef env))]
    [(with x e b) (with x (constant-propagation e env) (constant-propagation b (extend-env x e env)))]
    [(fun arg body) (closureV arg body env)]
    [(app f e) (app (constant-propagation f env) (constant-propagation e env))]
  ))

'ejercicio4
(test (constant-propagation
   (parse '{with {x 3} {+ x x}})) (parse '{with {x 3} {+ 3 3}}))
(test (constant-propagation
   (parse '{with {x 3} {with {y 5} {+ x y}}})) (parse '{with {x 3} {with {y 5} {+ 3 5}}}))
(test (constant-propagation
   (parse '{with {x 3} {with {y 5} {+ z z}}})) (parse '{with {x 3} {with {y 5} {+ z z}}}))

;ya no di mas :( , disculpe por no poder ir a clases 