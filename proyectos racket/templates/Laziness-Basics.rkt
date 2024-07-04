#lang lazy
; Para probar los ejemplos en eager volver a: lang plai

; Evaluacion Lazy / Perezosa
; Que cambia al evaluar las expresiones al final?

(define (sum x y z)
  (+ x y)
  )

(sum 2 7 (+ 1 #f))
; 1. En eval eager, esto falla porque los argumentos se evaluan
; antes de llamar a la funcion

; 2. En eval lazy no habra un error, pues los argumentos se
; evaluan solo cuando se necesitan, z no se usa, por lo que
; no llega al error.

; Como afecta esto a un lenguaje? Veamos dos casos

; IF, en un lenguaje eager normalmente no es una funcion
; pero en una implementacion lazy, es una funcion
(if #t 4 (+ 1 #f))
; En lazy, ejecutar esta linea no devuelve un resultado, sino
; una promesa, hasta que el resultado no se use, no lo va a
; evaluar. Promete hacerlo, algun dia.

; Sin embargo, en eager, el if previo tampoco va a fallar, esto
; es porque en eval eager, el if es un truco de parser.
(print (if #t 4 (+ 1 #f)))


; AND/OR
; Caso similar al del if, en un lenguaje eager, and y or no son
; funciones, pero en un lenguaje lazy si.

(and #f (+ 1 #t))
(or #t (+ 1 #t))


; CONS
; Los constructores clasicos en eager evaluan los argumentos
; en lazy no.

(cons 1 (+ 1 #t))

; En este caso, se devuelve un par de promesas que seran
; ejecutadas cuando sean usadas.

(define par (cons 1 (+ 1 #t)))
par
(car par)
;(cdr par)

#|
Basados en la idea que la ejecucion se retrasa lo mas posible,
incluso en los constructores, podemos crear listas infinitas

Pensemos en una lista infinita de 1
Diremos que
ones = (cons 1 ones)

Si vamos substituyendo y no evaluando, entonces tendremos
lo siguiente

ones = (cons 1 ones)
ones = (cons 1 (cons 1 ones))
ones = (cons 1 (cons 1 (cons 1 ones)))
ones = (cons 1 (cons 1 (cons 1 (cons 1 ones))))

y asi hasta el infinito.

Para poder ver la lista, tendremos que forzar su evaluacion
para esto haremos una funcion pick.
|#

(define (pick n list)
  (if (zero? n)
      empty
      (cons (car list) (pick (sub1 n) (cdr list)))))

; en la ultima linea, forzamos a evaluar el primero elemento
; de la lista. Pero no forzamos a evaluar la cola, por lo que
; puede seguir siendo infinita

(define ones (cons 1 ones))
(display (pick 15 ones))
(display (pick 4 ones))

; Recuerden que si probamos con numeros mas grandes, podriamos
; tener en algun momento problemas con el espacio, porque
; vamos a guardar muchos reemplazos en la ejecucion.

; Otra lista infinita son los numeros naturales

#|
nats = (cons 1 (map add1 '(1 2 3 4 5 6 7 ...)))

Pero aunque no tengamos la lista, no hay problema, pues no
vamos a evaluar hasta que sea necesario. 
|#
(define nats (cons 1 (map add1 nats)))
(display (pick 18 nats))
(display (pick 7 nats))

; Tambien se pueden hacer listas ciclicas
(define lst123 (append (list 1 2 3) lst123))
(display (pick 5 lst123))
(second lst123)
(fifth lst123)

; Todo esto es posible por la naturaleza de retrasar la
; evaluacion al maximo que tiene esta estrategia de evaluacion
; perezosa, en general esto se implementa en los lenguajes que
; lo soportan mediante promesas. 