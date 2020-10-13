#lang eopl
;Ready for test
(define copy
  (lambda (n x)
    (letrec
        (
        (lista
         (lambda (a b)
                 (if (= a 0)
                     empty
                     (cons b (lista (- a 1) b))
                 )
                )
               )
        )
      (lista n x)
      )))

(display (copy 4 (list 2 3)))