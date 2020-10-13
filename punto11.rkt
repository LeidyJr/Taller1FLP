#lang eopl
(define merge
  (lambda (LA LB)
  (cond
    [(null? LA) LB]    
    [(null? LB) LA]    
    [(< (car LA) (car LB))    
     (cons (car LA) (merge (cdr LA) LB))]     
    [else (cons (car LB) (merge LA (cdr LB)))])))