#lang eopl

(define factorial
  (lambda (n)
    ;(write n)
    (if
     (= n 0)
     1
     (* n (factorial (- n 1))))))

(define inicio
  (lambda (n)
    (if (even? n)
        2
        1)
    )
  )

(define list-facts-two
  (lambda (x)
    (letrec
        (
        (lista
         (lambda (a b)
                 (if (= a b)
                     (cons (factorial b) empty)
                     (cons (factorial a) (lista (+ a 2) b))
                 )
                )
               )
        )
      (lista (inicio x)x)
      )))
