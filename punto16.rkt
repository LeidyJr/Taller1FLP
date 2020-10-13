#lang eopl

(define bubble
  (lambda (L)
    (if (null? (cdr L))L  
        (if (< (car L) (cadr L)) 
            (cons (car L) (bubble (cdr L))) 
            (cons (cadr L) (bubble (cons (car L) (cddr L))))
            )
        )
    )
  )

(define bubble-aux
  (lambda (len List)    
    (cond ((= len 1) (bubble List))   
          (else (bubble-aux (- len 1) (bubble List))))))

(define (bubble-sort L) 
    (bubble-aux (length L) L))

