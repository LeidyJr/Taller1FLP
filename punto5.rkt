#lang eopl
(define (fibo x)

  (cond
    [(= x 0) 0]
    [(= x 1) 1]
    [else (+ (fibo (- x 1)) (fibo (- x 2)))]
))

(define list-fibo
  (lambda (x)
    (letrec
        (
        (lista
         (lambda (a b)
                 (if (= a b)
                     (cons (fibo b) empty)
                     (cons (fibo a) (lista (+ a 1) b))
                 )
                )
               )
        )
      (lista 0 x)
      )))
(display (list-fibo 6))