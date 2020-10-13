#lang eopl
(define filter-acum
  (lambda(a b F acum filter)
    (cond
      [(<= a b)(cond
                 [(filter a) (+ acum (filter-acum (+ a 1) b F a filter))]
                 [else (filter-acum (+ a 1) b F acum filter)])]
      [else acum])
    )
  )