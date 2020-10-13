#lang eopl

(define count-occurrences
  (lambda (x L)
    (cond
      [(eqv? L '()) 0]
      [(list? (car L)) (+ (count-occurrences x (car L)) (count-occurrences x (cdr L)))]
      [(eqv? x (car L))(+ 1 (count-occurrences x (cdr L)))]
      [else (count-occurrences x (cdr L))]
      )
    )
  )