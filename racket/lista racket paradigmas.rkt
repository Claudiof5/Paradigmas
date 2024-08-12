#lang racket
(define (concatenar1 l1 l2)
  (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [else (cons (first l1) (concatenar1 (rest l1) l2))]
  )
)

(define (concatenar2 ll)
  (cond
    [(null? ll) '()]
    [(list? (first ll))
     (concatenar1 (first ll) (concatenar2 (rest ll)))]
    [else (cons (ll) (concatenar2 (rest ll)))]
  )
)

(define (concatenar3 . l)
  (cond
    [(null? l) '()]
    [else (concatenar1 (first l) (apply concatenar3 (rest l)))]
  )
)

(define (concatenar4 . l)
  (apply concatenar2 l)
  )

(define (juntar l1 l2)
  (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [else (cons (first l1) (juntar l2 (rest l1)))]
  )
)

(define (adicionarFinal e l)
  (cond
    [(null? l) (list e)]
    [else (cons (first l)(adicionarFinal e (rest l)))]
    )
  )
(define (inverter L)
  (cond
    [(null? L) '()]
    [ else (adicionarFinal (first L) (inverter (rest L)))]  
   )
  )

(define (intercala n e1 e2)
  (cond
    [(zero? n) '()]
    [else (cons e1 (intercala (- n 1) e2 e1))]
    )
  )

(define (intercala2 n . l)
  (cond
    [(zero? n) '()]
    [(null? l) '()]
    [else (cons (first l) (apply intercala2 (- n 1) (adicionarFinal (first l) (rest l) )))]
    )
  )

(define (parear e l)
  (cond
   [(null? l) '()]
   [else (cons (cons e (list (first l))) (parear e (rest l)))]
   )
  )

(define (pares L)
  (cond
    [(null? L) '()]
    [else (concatenar1 ( parear (first L) (rest L)) (pares (rest L)))
     ]
   )
  )

(define (member? e L)
  (cond
    [(null? L)#f]
    [(eq? e (first L)) #t]
    [else (member? e (rest L))]
   )
 )

(define (conjunto? L)
  (cond
    [(null? L) #t]
    [(member? (first L) (rest L)) #f]
    [else (conjunto? (rest L))]
   )
  )

(define (prefixo? l1 l2)
  (cond
    [(null? l1) #t]
    [(null? l2) #f]
    [(not (eq? (first l1)(first l2))) #f]
    [else (prefixo? (rest l1) (rest l2))]
   )
 )

(define (subsequencia? l1 l2)
  (cond
    [(prefixo? l1 l2) #t]
    [(null? l2) #f]
    [else (subsequencia? l1 (rest l2))]
   )
  )

(define (iguais-lg? lg1 lg2)
  (cond
    [ (and (null? lg1) (null? lg2) )
      #t]
    [(and (list? (first lg1)) (list? (first lg2)))
     (and (iguais-lg? (first lg1) (first lg2)) (iguais-lg? (rest lg1) (rest lg2))) 
     ]
    [(eq? (first lg1) (first lg2) )
     (iguais-lg? (rest lg1) (rest lg2))]
    [else #f]
   )
  )

(define (substitui old new L)
(cond
  [(null? L) '()]
  [(eq? (first L) old) (cons new (substitui old new (rest L))) ]
  [ (cons (first L)(substitui old new (rest L)))]
  )
)

(define (member*? e L)
  (cond
    [(null? L)#f]
    [(list? (first L)) (or (member*? e (first L)) (member*? e (rest L)))]
    [(eq? e (first L)) #t]
    [else (member*? e (rest L))]
   )
 )
