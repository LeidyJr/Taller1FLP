#lang eopl

(define every?
  (lambda (P L)
    (cond
      [(eqv? L '()) #t]
      [(P(car L)) (every? P (cdr L))]
      [else #f]
    )
  ))