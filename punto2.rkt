#lang eopl
;Ready for test
(define list-tails
  (lambda (list)
    (letrec
        (
         (listar
          (lambda (lista)
            (if (eqv? lista '())
                empty
                (cons lista(listar (cdr lista)))
              )
            )
          )
         )(listar list)
      )
    )
  )

(display (list-tails '(1 a (e 4) 5 v)))