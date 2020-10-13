#lang eopl
(define zip
  (lambda (F L1 L2)
    (cond
    [(eqv? L1 '()) empty]
    [else (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2)))]
  )
    ))

