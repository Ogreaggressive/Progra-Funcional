#lang play

(define (even n)
  (match n
    [0 #t]
    [1 #f]
    [_ (odd (sub1 n))]
    )
  )

(define (odd n)
  (match n
    [0 #f]
    [1 #t]
    [_ (even (sub1 n))]
    )
  )

(even 1000)