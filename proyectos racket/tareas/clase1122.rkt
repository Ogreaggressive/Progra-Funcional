#lang play
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
    [field step 1])
   ;Methods
   ([method inc () (set! count (+ count step)) count]
    [method dec () (set! count (- count step)) count]
    [method reset () (set! count 0)]
    [method step! (v) (set! step v)]
    )
   )
  )

(counter4 'inc) ;1
(counter4 'step! 13)
(counter4 'inc) ;14
(counter4 'inc) ;27
(counter4 'inc) ;40
(counter4 'step! 10)
(counter4 'dec) ;30
(counter4 'reset)
(counter4 'dec) ;-10

; o.m(1,2)
; (→ o m 1 2)

(defmac (→ o m arg ...)
  (o 'm arg ...)
  )

(→ counter4 inc)
(→ counter4 inc)
(→ counter4 step! 2)
(→ counter4 inc)

(define (add n)
  (λ (m)
    (+ n m)))

(add 5)
(add 2)

(define (make-count [init-count 0] [init-step 1])
  (OBJECT
   ([field count init-count]
    [field step init-step])
   ;Methods
   ([method inc () (set! count (+ count step)) count]
    [method dec () (set! count (- count step)) count]
    [method reset () (set! count 0)]
    [method step! (v) (set! step v)]
    )
   )
  )

(let ([c1 (make-count 2 3)][c2 (make-count 6 1)])
     (+ (→ c1 inc) (→ c2 inc))
     )


(define (all-inc lst)
  (map (λ (o) (→ o inc)) lst))
(all-inc (list
          (make-count)
          (make-count 2 3)
          (OBJECT () ([method inc () "hello"]))))

(define (make-node l r
(OBJECT
 ([field left l]
 [field right r])
 ([method sum () (+ (→ left sum) (→ right sum))])))
)

(define (make-leaf )
  (OBJECT
   ([field value v])
   ([method sum () value]))
  )


(let ([tree (make-node
               (make-node (make-leaf 3)
                          (make-node (make-leaf 10)
                                     (make-leaf 4)))
               (make-leaf 1))])
   (→ tree sum))

