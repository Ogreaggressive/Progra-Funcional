#lang play
#|
<WAE> ::=    <num> | <bool> |<id>
          | (+ <WAE> <WAE>)
          | (- <WAE> <WAE>)
          | (with <id> <WAE> <WAE>)
          | (* '(<WAE>))
          | (withN ('<WAE>) <WAE>)     
          
|#
(deftype Expr
  [num n]
  [bool b]
  [add l r]
  [sub l r]
  [id sym]
  [with x ne b]
  [mult list]
  [withN varList b]
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
    [(cons '* tail) (mult (map parse tail))]
    [(list 'withN (cons head tail) body)
     (with (car head) (parse (car(cdr head)))
           (cond
             [(empty? tail) (parse body)]
             [else (parse(list 'withN tail body))]))]
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype Type
  (Num)
  (Bool))

;https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._andmap%29%29
;define all-num devuelve true si todos los valores son expresiones numericas, sino devuelve falso
(define (all-num? list)
  (andmap (λ (expr) (equal? (typeof expr) Num)) list))

(define (typeof expr)
  (match expr
    [(num n)(Num)]
    [(bool b)(Bool)]
    [(add l r)(let ([tl (typeof l)]
                    [tr (typeof r)])
                (if (and (Num? tl) (Num? tr))
                    (Num)
                    (error "error: incorrect type")))]
    [(sub l r) (if (and (Num? (typeof l)) (Num? (typeof r)))
                    (Num)
                    (error "error: incorrect type"))]
      [(mult list) (if (all-num? list)
                    (Num)
                    (error "error: incorrect type"))]
    )
  )

; {withN {x e}{y e}{z e} b}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parseMultiVariable varList body)
  (cond
    [(null? varList) null]
    [else
     (match (car varList)
       [(list id ne) with(cons (cons id (parse ne)) (parseMultiVariable (cdr varList)))]
       [else (error "error: una o mas variables estan incompletas")])]
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
    [(mult list1) (foldr (λ (val1 acc) (* (interp val1) acc)) 1 list1)]
))


(define (runChecker prog ) ;typechecker
  (let* ([expr (parse prog)]
         [t (typeof expr)]
         [result (interp expr)])
    result))

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


#|
1. * infinita TypeChecker logica explicado por chatGPT
              Debatido con Rodrigo Guardia
Modifica el lenguaje para que soporte multiplicación infinita y actualize el type checking (2pts)
(test (run '{* 1 1 1 1}) 1)
(test/exn (run '{* 1 #t 1 1}) "error: incorrect type")
|#

'Ejercicio1

(test (run '{* 1 1 1 1}) 1)
(test (run '{* 1 2 3 4 5}) 120)
(test (run '{* }) 1)
(test/exn (runChecker '{* 1 #t 1 1}) "error: incorrect type")

#|
2. withN (3pts) Hecho con ayuda de Rodrigo Guardia
Actualmente, el lenguaje WAE soporta with con sólo una variable:
{with {x 2} {+ x 4}}

Para manejar dos variables, necesitamos anidar with
{with {x 2} {with {y 3} {with {z 1} {+ x {+ y z}}}}}

Haz las modificaciones necesarias para que pueda soportar  withN de la siguiente manera.
{withN {{x 2} {y 3} {z 1}} {+ x {+ y z}}}

Escribe pruebas para demostrar tu implementación
|#

'Ejercicio2

(test (run '{withN {{x 2} {y 3} {z 1}} {+ x {+ y z}}}) 6)
(test (run '{withN {{a 1} {b 2} {c 3} {d 4}} {+ a {+ b {+ c d}}}}) 10)
(test (run '{withN {{x 2} {y 3} {z 1}} {with {a 4} {+ x {+ y a}}}}) 9)
(test (run '{withN {{x 2} {y 3} {z 1}} {with {x 10} {+ x {+ y z}}}}) 14)
(test (run '{withN {{x 2} {y 3} {z 1}} {with {x 10} {+ y {+ z x}}}}) 14)

#|
3. free-vars (3pts)
Ahora que ya sabemos qué son las ocurrencias libres, también sabemos que si al interpretar nos encontramos con una de ellas
esperamos un error (pues esto quiere decir que no han sido substituidas). Crea la función (free-vars expr) que recibe una
expresión y devuelve la lista de ocurrencias libres de la expresión:


(test (free-vars (parse '{+ x {+ z 3}})) '(x z))
(test (free-vars (parse 'x)) '(x))
 Escribe pruebas para casos con with.
|#
;https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._remove%29%29
(define (free-vars expr)
  (match expr
    [(num n) '()]
    [(bool b) '()]
    [(id x) (list x)]
    [(add l r) (append (free-vars l) (free-vars r))]
    [(sub l r) (append (free-vars l) (free-vars r))]
    [(with id ne b) (append (remove id (free-vars b)) (free-vars ne))] 
    [(mult list1) (foldr (λ (val acc) (append (free-vars val) acc)) '() list1)] 
    [else '()]))

'Ejercicio3

(test (free-vars (parse '{+ x {+ z 3}})) '(x z))
(test (free-vars (parse 'x)) '(x))
(test (free-vars (parse '{with {x 3} {+ x 1}})) '())
(test (free-vars (parse '{with {y 2} {+ x y}})) '(x))
(test (free-vars (parse '{with {x 3} {with {y x} {+ x y}}})) '(x)) 

#|
4. count-nums (2pts)
Sobre el AST, opera distintas funciones, en particular, las funciones de analyze recorren el árbol y extraen información o
alguna propiedad interesante. Ahora implementarás una función simple de analyze, (count-nums expr) que recorre una expresión
y devuelve la cantidad de constantes numéricas en la expresión.

(test (count-nums (add (num 3) (num 2))) 2)

Escribe pruebas para la función e impleméntala. 
|#

;no utilizo bool y id porque no trabajan con nums
(define (count-nums expr)
  (match expr
    [(num n) 1]
    [(add l r) (+ (count-nums l) (count-nums r))]
    [(sub l r) (+ (count-nums l) (count-nums r))]
    [(with id ne b) (+ (count-nums ne) (count-nums b))]
    [(mult list1) (foldr (λ (val acc) (+ (count-nums val) acc)) 0 list1)]
    [else 0]))

'Ejercicio4

(test (count-nums (add (num 3) (num 2))) 2)
(test (count-nums (mult (list (num 1) (num 2) (num 3)))) 3)
(test (count-nums (sub (num 5) (add (num 2) (num 1)))) 3)
(test (count-nums (num 7)) 1)
(test (count-nums (id 'x)) 0)
(test (count-nums (parse '{with {x 2} {+ x 4}})) 2)
