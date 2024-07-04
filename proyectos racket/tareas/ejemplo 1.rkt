#lang play
"hello world"
;This is a comment
#|
aasxafsafasfgea
|#
;Primitives
;Numbers
1
1.45
1+2i
(sqrt -4)
4/3
6/8

; Booleans
#t
#f

;Strings
"hola"

;Symbols
'hola

;Functions

(+ 1 2)
+ 1 2

(* 1 2 3 4 5)
(and #t #f)
(and 2 3)

(and 3 2)
(or 3 6)

(string-append "hool" "a")
(string->symbol "hola")
(number? 45)

(printf "hola")

;imprima la raiz cuadrada de -1/4
(printf (number->string (sqrt -1/4)))

#|
if(cond)
  ifTrue
  ifFalse

(if cond ifTrue ifFalse)


(cond [? ans!]
      [? ans2]
      [else ans3]
|#

(if (> 10 4)
    "hola"
    (if (< 3 5)
        "chao"
        #f))

(cond [(> 10 4) "hola"]
      [(< 3 5) "chao"]
      [else #f])

(define x 10)
x

(let ([x 2] [y 3]) (+ x y))

(let* ([a 3] [b (+ a 2)]) (+ a b))

(define (double x)
  (+ x x))

(double 4)

(define (isEven? num)
  (if (eq? (modulo num 2) 0)
      #t
      #f))

(test (isEven? 4) #t)