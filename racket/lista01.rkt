#lang racket
(provide concatenar1
 concatenarInv
 concatenar2
 concatenar3
 juntar
 intercala
 intercala2
 parear
 pares
 conjunto?
 prefixo?
 subsequencia?
 iguais-lg?
 substitui-lg
 aplanar )


; (concatenar1 l1 l2)
;  Concatena duas listas
; (concatenar1 list? list?) -> list

(define (concatenar1 l1 l2)
  (if (null? l1)
      l2
      (cons (first l1) (concatenar1 (rest l1) l2))
      )
  )

; (concatenarInv l1 l2)
;  Concatena de maneira inversa duas listas
; (concatenarInv list? list?) -> list
(define (concatenarInv l1 l2)
  (concatenar1 l2 l1)
  )

; (concatenar2 l1)
;  Concatena as listas dentro de uma lista
; (concatenar2 list?) -> list

(define (concatenar2 l1)
  (if (null? l1)
      '()
      (concatenar1 (first l1) (concatenar2 (rest l1)))
      )
  )

; (concatenar3 l . args)
;  Concatena uma quantidade qualquer de listas
; (concatenar3 list? ...) -> list

(define (concatenar3 . args)
  (if (null? args)
      '()
      (concatenar1 (first args) ( apply concatenar3 (rest args)))
      )
  )

; (juntar l1 l2)
;  Junta duas listas, intercalando seus elementos.
; (juntar list? list?) -> list
(define (juntar l1 l2)
  (if (null? l1)
      l2
      (cons (first l1) (juntar l2 (rest l1)))
      )
  )

; (intercala n e1 e2)
;  Cria uma lista intercalada de tamanho N, de dois atomos e1 e e2
; (intercala integer? atom? atom?) -> list

(define (intercala n e1 e2)
  (if (<= n 0)
      '()
      (cons e1 (intercala (- n 1) e2 e1))
      )
  )

; (intercala2 n e ...)
;  Cria uma lista intercalada de tamanho N, de uma quantidade arbitraria de atomos
; (intercala2 integer? atom? ...) -> list
(define (intercala2 n . e)
  (if (or (<= n 0)
          (null? e)
          )
      '()
      (cons (first e) (apply intercala2 (- n 1) (concatenar1 (rest e) (list (first e)) )))
      )
  )
; (parear e l)
;  Produz a lista de pares cujo primeiro elemento é E e o segundo elemento é um membro de L.
; (parear atom? list?) -> list
(define (parear e l)
  (if (null? l)
      '()
      (cons (list e (first l)) (parear e (rest l)))
      )
  )
; (pares l)
; Produz a lista de todos os pares de elementos de L
; (pares list?) -> list
(define (pares l)
  (if (null? l)
      '()
      (concatenar1 (parear (first l) (rest l)) (pares (rest l)))
      )
  )
; (conjunto? l)
; Retorna verdadeiro caso seja um conjunto e falso caso não seja
; (conjunto? list?) -> boolean
(define (conjunto? l)
  (cond [(or (null? l)
             (null? (rest l))
             )
         #t]
        
        [(eqv? (first l) (first (rest l)))
         #f]
        
        [(conjunto? (cons (first l) (rest (rest l))))
         (conjunto? (rest l))]
        
        [else #f]
        )
  )
; (prefixo? l1 l2)
; Retorna verdadeiro caso l1 seja prefixo de l2 e falso caso não seja
; (prefixo? list? list?) -> boolean
(define (prefixo? l1 l2)
  (cond [(null? l1) #t]
        
        [(null? l2) #f]
        
        [(eqv? (first l1) (first l2)) (prefixo? (rest l1) (rest l2))]
        
        [else #f]
    )
  )
; (subsequencia? l1 l2)
; Retorna verdadeiro caso l1 seja uma subsequencia de l2 e falso caso não seja
; (subsequencia? list? list?) -> boolean
(define (subsequencia? l1 l2)
  (cond [(null? l2) #f]
        
        [(prefixo? l1 l2) #t]
        
        [else (subsequencia? l1 (rest l2))]
    )
  )
; (iguais-lg? l1 l2)
; Retorna verdadeiro caso a lista generica l1 seja igual a l2 e falso caso não seja
; (iguais-lg? list? list?) -> boolean
(define (iguais-lg? lg1 lg2)
  (cond [(and (null? lg1)
              (null? lg2)
              )
         #t]

        [(and (null? lg1)
              (not (null? lg2))
              )
         #f]
        
        [(and (not (null? lg1))
              (null? lg2)
              )
         #f]
        
        [(and (list? (first lg1))
              (list? (first lg2))
              )
         (and (iguais-lg? (first lg1)(first lg2))
              (iguais-lg? (rest lg1) (rest lg2))
              )
         ]
        
        [(and (not (list? (first lg1)))
              (not (list? (first lg2)))
              )
         (and (eqv? (first lg1) (first lg2))
              (iguais-lg? (rest lg1) (rest lg2))
              )]
        
        [else #f]
        )
  )
; (substitui-lg old new lg)
; Substitui todas as ocorrências de old por new em uma lista genérica lg
; (substitui-lg atom? atom? list?) -> list
(define (substitui-lg old new lg)
  (cond [(null? lg) '()]
    
        [(list? (first lg))
         (cons (substitui-lg old new (first lg))
               (substitui-lg old new (rest lg))
               )]
    
        [(eqv? old (first lg))
         (cons new (substitui-lg old new (rest lg)))]
    
        [else (cons (first lg) (substitui-lg old new (rest lg)))]
        )
  )
; (aplanar lg)
; Recebe uma lista genérica e torna ela plana
; (aplanar list?) -> list
(define (aplanar lg)
  (cond [(null? lg) '()]
        
        [(list? (first lg))
         (concatenar1 (aplanar (first lg)) (aplanar (rest lg)))]
        
        [else (cons (first lg) (aplanar (rest lg)))]
        )
  )