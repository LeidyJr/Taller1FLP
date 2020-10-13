#lang eopl
(define sublist
  (lambda (L i j)
    (letrec
        (
         (listar-i
          (lambda (x lx)
            (if (= x 0)
                lx
                (listar-i (- x 1) (cdr lx)))))
         (listar-j
          (lambda (y ly)
            (if (= y 0)
                empty
                (cons (car ly) (listar-j (- y 1) (cdr ly))))))
         )
      (listar-i i(listar-j j L))
      )
          
    )
)
(display (sublist '(1 2 3 4 5) 2 4))