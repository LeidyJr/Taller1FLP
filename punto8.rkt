#lang eopl

(define flatten
  (lambda (L)
    (cond
      [(eqv? L '()) empty]
      [(list? L) (append (flatten (car L)) (flatten (cdr L)))]
      [else (list L)]
      )
    )
  )

