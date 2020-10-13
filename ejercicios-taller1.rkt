#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integrantes                                   ;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anacona Julián Fernando - 2027790-3743        ;;
;; Angel Juan José - 1825116-3743                ;;
;; Rivera Leidy Johanna - 2024011-3743           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Punto 1
;; copy: 
;; Proposito: Procedimiento que retorna una lista con n ocurrencias de un elemento x
(define copy
  (lambda (n x)
    (if (= n 0)
        empty
        (cons x (copy (- n 1) x)))))
; pruebas:
(copy 7 'seven)
(copy 4 (list 4 5 6))
(copy 0 (list 9 8 7))
;Punto 2
;; list-tails:
;; Proposito: Procedimiento que retorna en una lista todas las sublistas de los elementos consecutivos de la lista
(define list-tails
  (lambda (L)
    (if (null? L)
        empty
        (cons L (list-tails (cdr L))))))
;pruebas:
(list-tails '(1 a (e 4) 5 v))
(list-tails '(3 (x y) 2))
(list-tails '(3 2 j 5 7))

;Punto 3
;; sublist:
;; Proposito: retornar la sublista entre inicio y final
(define sublist
  (lambda (lista inicio final)
    (cond
      [(and (= inicio 0) (> final -1)) (cons (car lista) (sublist (cdr lista) inicio (- final 1)))]
      [(or (= final -1) (eqv? lista '())) empty]
      [else (sublist (cdr lista) (- inicio 1) (- final 1))]
      )
  )
 )
;pruebas:
(sublist '( c a (b c) 9 d s) 3 4)
(sublist '((1 x) v b f d 9) 0 3)
(sublist '((1 x) v b f d 9) 0 0)


;Punto 4
;; exists?:
;; Proposito: Retorna #t si alguno de los elementos de la lista satisface el predicado, de lo contrario retorna #f
(define exists?
  (lambda (predicado lista)
    (cond
      [(eqv? lista '()) #f]
      [(predicado (car lista)) #t]      
      [else (exists? predicado (cdr lista))]
      )
    )
  )
;pruebas:
(exists? number? '(x e s d))
(exists? symbol? '(a b c))
(exists? number? '(1 empty empty b))
;Punto 5
;; Función auxiliar: fibonacci
;; Proposito: Crea la sucesión de fibonacci
(define fibonacci
  (lambda (n)
    (cond
      [(= n 0) 0]
      [(= n 1) 1]
      [else (+ (fibonacci (- n 1)) (fibonacci (- n 2)))]
      )
    )
  )
;pruebas:
(fibonacci 2)
(fibonacci 5)
(fibonacci 9)
;; list-fibo:
;; Proposito: Crea una lista con cada número de la sucesión
(define list-fibo
  (lambda (n)
    (letrec
        (
         (lista (lambda (o f)
                  (if (= o f)
                      (cons (fibonacci f) empty)
                      (cons (fibonacci o) (lista (+ o 1) f))
                      )
                  )
                )
         )
      (lista 0 n)
    )
  )
)
;pruebas:
(list-fibo 4)
(list-fibo 7)
(list-fibo 9)
;Punto 6
;; Función auxiliar: factorial
;; Propósito: Calcular el factorial de n
(define factorial
  (lambda (n)
    (if
     (= n 0)
     1
     (* n (factorial (- n 1))))))
;pruebas:
(factorial 3)
(factorial 5)
(factorial 9)
;; Función auxiliar: inicio
;; Propósito: Verificar si el número n es par o impar, dependiendo de esto retorna en qué número debe iniciar el factorial
(define inicio
  (lambda (n)
    (if (even? n)
        2
        1)
    )
  )
;pruebas:
(inicio 3)
(inicio 2)
(inicio 5)
;list-facts-two
;; Propósito: Genera una lista de factoriales, de 2 en 2, tomando como punto de partida el dado por la función anterior.
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
;pruebas:
(list-facts-two 4)
(list-facts-two 5)
(list-facts-two 9)
;Punto 7
;; count-occurrences:
;; Propósito: Retornar el número de ocurrencias de un elemento en la lista
(define count-occurrences
  (lambda (simbolo lista)
    (cond
      [(eqv? lista '()) 0]
      [(list? (car lista)) (+ (count-occurrences simbolo (car lista)) (count-occurrences simbolo (cdr lista)))]
      [(eqv? simbolo (car lista)) (+ 1 (count-occurrences simbolo (cdr lista)))]
      [else (count-occurrences simbolo (cdr lista))]
      )
    )
  )
;pruebas:
(count-occurrences 'x '((f x) y (((x z) () x))))
(count-occurrences "happy" '( 7 8 5 "happy" ("happy" "happy")))
(count-occurrences '(1 2) '( (1 2) ( 1 (1 2))))
;Punto 8
;; flatten:
;; Propósito: Recibe una lista y devuelve la lista sin los paréntesis internos.
(define flatten
  (lambda (lista)
    (cond
      [(eqv? lista '()) empty]
      [(list? lista) (append (flatten (car lista)) (flatten (cdr lista)))]
      [else (list lista)]
      )
    )
  )
;pruebas:
(flatten '((x) () (3 ()) () (c)))
(flatten '((a b c  (d e f)())))
(flatten '((z b y (a)(e j 2)(3)  (t e i)())))
;Punto 9
;; every?:
;; Propósito: Retorna #t si todos los elementos de la lista cumplen con el predicado, de lo contrario retorna #f.
(define every?
  (lambda (predicado lista)
    (cond
      [(eqv? lista '()) #t]
      [(predicado (car lista)) (every? predicado (cdr lista))]
      [else #f]
      )
    )
  )
;pruebas:
(every? symbol? '(q w 3 r t y))
(every? number? '(9 8 6 4 7))
;Punto 10
;; upside-down:
;; Propósito: Dado un número n, devolverlo con los dígitos al revés
(define upside-down
  (lambda (numero)
    (cond
      [(> numero 0) (display (remainder (truncate numero) 10)) (upside-down (truncate (/ numero 10)))]
      )
    )
  )
;pruebas:
(upside-down 456)
(upside-down 98765)
; Punto 11
;; merge: 
;; Propósito: Dadas dos listas ordenadas, devolver una lista ordenada con los elementos de ambas.
(define merge
  (lambda (l1 l2)
    (cond
      [(eqv? l1 '()) l2]
      [(eqv? l2 '()) l1]
      [(< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2))]
      [else (cons (car l2) (merge l1 (cdr l2)))]
      )
    )
  )
;pruebas:
(merge '(2 6 7) '(1 3 5))
(merge '(7 8 9) '(1 4 5))
; Punto 12
;; zip:
;; Propósito: Retornar una lista donde la n-ésima posición corresponde a aplicar la función F sobre el n-ésimo elemento de las listas L1 y L2
(define zip
  (lambda (F L1 L2)
    (cond
    [(eqv? L1 '()) empty]
    [else (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2)))]
  )
    ))
;pruebas:
(zip + '(11 5 6) '(10 9 8))
(zip - '(11 5 6) '(10 9 8))
;Punto 13
;; filter-acum:
;; Propósito: Aplica la función binaria F a todos los elementos que esta ́an en el intervalo [a, b] que cumplen con el predicado de la funcion filter, retornando el valor acumulado en acum.
(define filter-acum
  (lambda(a b F acum filter)
    (cond
      [(<= a b)(cond
                 [(filter a) (+ acum (filter-acum (+ a 1) b F a filter))]
                 [else (filter-acum (+ a 1) b F acum filter)])]
      [else acum])
    )
  )
;pruebas:
(filter-acum 1 5 + 0 odd?)
(filter-acum 1 7 + 0 even?)
;Punto 14
;; Función auxiliar: merge-lists
;; Propósito: Une las listas ordenadas de acuerdo al criterio de comparación, basado en merge-sort.
(require racket/list)
(define merge-lists
  (lambda (l1 l2 f)
    (cond
      [(eqv? l1 '()) l2]
      [(eqv? l2 '()) l1]
      [(f (car l1) (car l2)) (cons (car l1) (merge-lists (cdr l1) l2 f))]
      [else (cons (car l2) (merge-lists l1 (cdr l2) f))]
      )
    )
  )
;pruebas:
(merge-lists '(2 1) '(3 2 1 1) >)
(merge-lists '(1) '(1 2 3) <)
;; sort:
;; Propósito: Retornar una lista ordenada de acuerdo al criterio de orden
(define sort
  (lambda (l f)
    (cond
      [(or (null? l) (null? (cdr l))) l]
      [(null? (cddr l)) (merge-lists (list (car l)) (cdr l) f)]
      [else (let ([x (ceiling (/ (length l) 2))]) (merge-lists (sort (take l x) f) (sort (drop l x) f) f))]
      )
    )
  )
;pruebas:
(sort '(9 5 7 3 5 1) <)
 (sort '(9 5 7 3 5 1) >)
; Punto 15
;; hermite:
;; Propósito: Dado un orden n y una abcisa x, retorna el resultado del cálculo del polinomio de Hermite.
(define hermite
  (lambda (n x)
  (cond
    [(= 0 n) 1]
    [(= 1 n) (* 2 x)]
    [else (- (*(* 2 x) (hermite (- n 1) x)) (* (* 2 (- n 1)) (hermite (- n 2) x)))]
  )
))
;pruebas:
(hermite 3 6)
(hermite 2 5)
; Punto 16:
;; Función auxiliar: bubble
;; Propósito: Algoritmo bubble-sort, el cual:
(define bubble
  (lambda (L)
    (if (null? (cdr L)) L  ;; Si no hay elementos a la derecha, retorna la lista como está,   
        (if (< (car L) (cadr L)) ;; Toma el primer elemento y verifica si es menor que el siguiente
            (cons (car L) (bubble (cdr L))) ;; Si es menor, se deja el elemento en la posición que está y se sigue ejecutando con el resto de la lista
            (cons (cadr L) (bubble (cons (car L) (cddr L))));; Si es mayor, el primer elemento de la lista a generar será el segundo de la lista ingresada, se saca de la lista y se llama el algoritmo con una lista conformada por el car y el cddr
            )
        )
    )
  )
;pruebas:
(bubble '(8 2 5 2 3))
(bubble '(5 10 9 8 7))
;; Función auxiliar: ordenada?
;; Propósito: Verificar si la lista ya está ordenada
(define (ordenada? l)
  (cond
    [(null? (cdr l)) #t]
    [(> (car l) (cadr l)) #f]
    [else (ordenada? (cdr l))]
    )
  )
;pruebas:
(ordenada? '(8 2 5 2 3))
(ordenada? '(5 10 9 8 7))
;; bubble-sort:
;; Propósito: 
(define bubble-sort
  (lambda (l)
    (letrec
        (
         (b-sort
          (lambda (l boolean)
            (cond
              [(eqv? boolean #f) (b-sort (bubble l) (ordenada? (bubble l)))]
              [else l]
              )
            )
          )
         )
      (b-sort l #f)
      )
    )
  )
;pruebas:
(bubble-sort '(8 2 1 6 8))
(bubble-sort '(8 2 5 2 3))
; Punto 17
;; path:
;; Propósito: Dado un numero n y un árbol binario, retorna una lista con la ruta a tomar
;; indicada por cadenas left y right, hasta llegar al numero n. Retorna vacio si n está en el nodo raíz.
(define path (lambda (n tree)
              (letrec (
                       (a-path (lambda (  a-tree)
                          (cond [(null? a-tree) (cons "not-found" empty)]
                             [(= n (car a-tree)) empty ]
                             [(< n (car a-tree)) (cons "left" (a-path  (cadr a-tree)))]
                             [(> n (car a-tree)) (cons "right" (a-path  (caddr a-tree)))]))))
               (if (equal? (car (reverse (a-path tree)))"not-found") '() (a-path tree)) ) ))
;pruebas:
(path 13 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
(path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ())()) (31 () ()))))