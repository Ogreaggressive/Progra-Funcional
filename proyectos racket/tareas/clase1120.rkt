#lang play
#|
(define (obj x)
  (lambda () x))
; object --> obj
; Fields --> x
(define counter
  (let ([count 0])
    (lambda () (begin (set! count (add1 count)) count)))
  )

(define counter1
  (let ([count 0])
    (lambda (msg)
      (match msg ; dispatch
        ['inc (begin (set! count (add1 count)) count)]
        ['dec (begin (set! count (sub1 count)) count)]
        ['reset (set! count 0)]
        ))
  ))

#|(counter1 'inc)
(counter1 'inc)
(counter1 'dec)
(counter1 'reset)
(counter1 'dec)|#

(define counter2
  (let ([count 0]
        [step 1])
    (lambda (msg . args) ; recibe 1 o mas
      (match msg ; dispatch
        ['inc (begin (set! count (+ count step)) count)]
        ['dec (begin (set! count (- count step)) count)]
        ['reset (set! count 0)]
        ['step! (set! step (first args))]
        ))
  ))

#|(counter2 'inc)
(counter2 'inc)
(counter2 'step! 10)
(counter2 'dec)
(counter2 'reset)
(counter2 'dec)|#

(define counter3
  (let ([count 0]
        [step 1])
    ;methods
    (let ([methods (list
                    (cons 'inc (lambda () (set! count (+ count step)) count))
                    (cons 'dec (lambda () (set! count (- count step)) count))
                    (cons 'reset (lambda () (set! count 0)))
                    (cons 'step! (lambda (args) (set! step args)))
                    )
                          ])
      (lambda (msg . args)
        (let ([found (assoc msg methods)])
          (if found
              (apply (cdr found) args)
              (error "Not understood method: " msg)
              )
          )
        )
      )
  ))

#|(counter3 'inc)
(counter3 'inc)
(counter3 'step! 10)
(counter3 'dec)
(counter3 'reset)
(counter3 'dec)|#
|#


(defmac (OBJECT
         ([field fname fval] ...) ; 0 o mas
         ([method mname mparams mbody ...] ...))
         #:keywords field method
         (let ([fname fval] ...)
           (let ([methods (list
                    (cons 'mname (λ mparams mbody ...)) ...
                    )])
             (λ (msg . args)
               (let ([found (assoc msg methods)])
                 (if found
                     (apply (cdr found) args)
                     (error "Not understood method: " msg)
                     )
                 )
               )
             )
  ))
  


(define counter4
  (OBJECT
   ([field count 0]
    [field step 1]
    [field checkpoint 0])
   ;Methods
   ([method inc () (set! count (+ count step)) count]
    [method dec () (set! count (- count step)) count]
    [method reset () (set! count 0)]
    [method step! (v) (set! step v)]
    [method checkpoint (v) (set! count v)]
    )
   )
  )

#|(counter4 'inc) ;1
(counter4 'step! 13)
(counter4 'inc) ;14
(counter4 'inc) ;27
(counter4 'inc) ;40
(counter4 'step! 10)
(counter4 'dec) ;30
(counter4 'reset)
(counter4 'dec)|#


(define cuenta
  (OBJECT
   ([field name "pobre"]
    [field impuesto 0.13]
    [field saldo 0]
    )
   ;Methods
   (
    [method deposit (v) (set! saldo (+ saldo v))]
    [method withdraw (v) (set! saldo (- saldo v))]
    [method balance () saldo]
    [method impuesto () (set! saldo (* saldo impuesto)) saldo]
    )
   )
  )

(cuenta 'deposit 300)
(cuenta 'withdraw 200)
(cuenta 'balance)
(cuenta 'impuesto)

