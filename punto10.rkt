#lang eopl
(provide (all-defined-out))

(define upside-down
     (lambda (n)
       (cond
         [(> n 0) (display (remainder (truncate n) 10)) (upside-down (truncate (/ n 10)))]
         )
       )
  )

