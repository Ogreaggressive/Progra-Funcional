#lang play
(define (while cond body)
  (if (cond)
      (begin
        (body
)        (while cond body)
        )
      (void)
      )
  )

(define x 10)

(while (λ ()(positive? x)) ; thunk
       (λ ()(begin (printf "x: ~a~n" x)
              (set! x (sub1 x)))))
; (while #t (void))