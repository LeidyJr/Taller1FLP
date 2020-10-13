#lang eopl
(define hermite
  (lambda (n x)
  (cond
    [(= 0 n) 1]
    [(= 1 n) (* 2 x)]
    [else (- (*(* 2 x) (hermite (- n 1) x)) (* (* 2 (- n 1)) (hermite (- n 2) x)))]
  )
))
