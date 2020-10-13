#lang eopl

(define path (lambda (n tree)
              (letrec (
                       (a-path (lambda (  a-tree)
                          (cond [(null? a-tree) (cons "not-found" empty)]
                             [(= n (car a-tree)) empty ]
                             [(< n (car a-tree)) (cons "left" (a-path  (cadr a-tree)))]
                             [(> n (car a-tree)) (cons "right" (a-path  (caddr a-tree)))]))))
               (if (equal? (car (reverse (a-path tree)))"not-found") '() (a-path tree)) ) ))
