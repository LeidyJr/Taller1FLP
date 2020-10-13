#lang eopl

(define exists?
  (lambda (P L)
    (cond
      [(eqv? L '()) #f]
      [(P(car L)) #t]
      [else (exists? P (cdr L))]
    )
  ))

(display (exists? number? '(a b c 2)))


