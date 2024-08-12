#lang racket

; Fernando Franco de Lacerda Neto

(provide CONJUNTO-VAZIO
         conjunto-vazio?
         conjunto?
         cardinalidade-c
         iguais-c?
         subconjunto?
         intersecta?
         intersecao
         uniao
         dif-conj
         dif-sim
         intersecao-lc
         uniao-lc
         dif-sim-lc
         ocorre-uma-vez-lc
         )

(define (any? x)
  #t)


(define CONJUNTO-VAZIO '())

; (conjunto-vazio? conj)
;  Checa se o conjunto é o conjuto vazio
; (conjunto-vazio? conjunto? ) -> boolean
(define/contract (conjunto-vazio? conj) (-> list? boolean?)
  (null? conj)
  )

; (conjunto? l)
;  Checa se a lista é um conjunto
; (conjunto? list? ) -> boolean
(define/contract (conjunto? l) (-> list? boolean?)
  (cond [(or (null? l)
             (null? (rest l))
             )
         #t]
        
        [(eqv? (first l) (first (rest l)))
         #f]
        
        [(conjunto? (cons (first l) (rest (rest l))))
         (conjunto? (rest l))
         ]
        
        [else #f]
        )
  )

; (lista-conjunto? l)
;  Checa se a lista em que todos os elementos sao conjuntos
; (lista-conjunto? list?) -> boolean
(define/contract (lista-conjunto? l) (-> list? boolean?)
  (cond [(null? l) #f]
        
        [(and (conjunto? (first l))
              (null? (rest l))) #t]
        
        [(not (conjunto? (first l))) #f]

        [else (lista-conjunto? (rest l))]
        
        )
  )

; (in e set)
;  Cehca se um elemento esta no conjunto
; (in atom? conjunto? ) -> boolean
(define/contract (in e set) (-> any? conjunto? boolean?)
  (cond [(null? set) #f]
        
        [(equal? e (first set)) #t]
        
        [else (in e (rest set))]
        )
  )

; (cardinalidade-c conj)
;  Retorna a cardinalidade do conjunto conj
; (cardinalidade-c conjunto? ) -> integer
(define/contract (cardinalidade-c conj) (-> conjunto? integer?)
  (if (null? conj)
      0
      (+ 1 (cardinalidade-c (rest conj)))
      )
  )

; (iguais-c? conj1 conj2)
;  Checa se conj1 e conj2 são iguais
; (iguais-c? conjunto? conjunto?) -> boolean
(define/contract (iguais-c? conj1 conj2) (-> conjunto? conjunto? boolean?)
  (cond [(and (null? conj1)
              (null? conj2)
              )
         #t]

        [(and (null? conj1)
              (not (null? conj2))
              )
         #f]
        
        [(in (first conj1) conj2)
         (iguais-c? (rest conj1) (remove (first conj1) conj2))]

        [else #f]
        )
  )

; (subconjunto? c1 c2)
;  Checa se c1 é subconjunto de c2
; (subconjunto? conjunto? conjunto? ) -> boolean
(define/contract (subconjunto? c1 c2) (-> conjunto? conjunto? boolean?)
  (cond [(null? c1) #t]
        
        [(in (first c1) c2)
         (subconjunto? (rest c1) c2)]

        [else #f]
        )
  )

; (intersecta? c1 c2)
;  Checa se há interseção entre c1 e c2
; (intersecta? conjunto? conjunto? ) -> boolean
(define/contract (intersecta? c1 c2) (-> conjunto? conjunto? boolean?)
  (cond [(or (null? c1)
             (null? c2))
         #f]
        
        [(in (first c1) c2) #t]

        [else (intersecta? (rest c1) c2)]
        )
  )

; (intersecao c1 c2)
;  Retorna a interseção entre c1 e c2
; (intersecao conjunto? conjunto? ) -> conjunto
(define/contract (intersecao c1 c2) (-> conjunto? conjunto? conjunto?)
  (cond [(or (null? c1)
             (null? c2))
         '()]
        
        [(in (first c1) c2)
         (cons (first c1) (intersecao (rest c1) c2))]

        [else (intersecao (rest c1) c2)]
        )
  )

; (uniao c1 c2)
;  Retorna a união entre c1 e c2
; (uniao conjunto? conjunto? ) -> conjunto
(define/contract (uniao c1 c2) (-> conjunto? conjunto? conjunto?)
  (cond [(null? c1) c2]

        [else (cons (first c1)
                    (uniao (rest c1)
                           (remove (first c1) c2)
                           )
                    )]
        )
  )

; (dif-conj1 c1 c2)
;  Retorna a diferença entre c1 e c2
; (dif-conj1 conjunto? conjunto? ) -> conjunto
(define/contract (dif-conj1 c1 c2) (-> conjunto? conjunto? conjunto?)
  (remove* (intersecao c1 c2) c1)
  )

; (dif-conj c1 c2)
;  Retorna a diferença entre c1 e c2
; (dif-conj conjunto? conjunto? ) -> conjunto
(define/contract (dif-conj c1 c2) (-> conjunto? conjunto? conjunto?)
  (cond [(null? c1) '()]

        [(in (first c1) c2)
         (dif-conj (rest c1) c2)]

        [else (cons (first c1)
                    (dif-conj (rest c1) c2))]
        )
  )

; (dif-sim1 c1 c2)
;  Retorna a diferença simétrica entre c1 e c2
; (dif-sim1 conjunto? conjunto? ) -> conjunto
(define/contract (dif-sim1 c1 c2) (-> conjunto? conjunto? conjunto?)
  (append (remove* (intersecao c1 c2) c1)
          (remove* (intersecao c1 c2) c2)
          )
  )

; (dif-sim c1 c2)
;  Retorna a diferença simétrica entre c1 e c2
; (dif-sim conjunto? conjunto? ) -> conjunto
(define/contract (dif-sim c1 c2) (-> conjunto? conjunto? conjunto?)
  (cond [(and (null? c1)
              (null? c2))
         '()]

        [(null? c1) c2]

        [(in (first c1) c2)
         (dif-sim (rest c1) (remove (first c1) c2))]

        [else (cons (first c1)
                    (dif-sim (rest c1) c2)
                    )]
        )
  )



; (intersecao-lc ...)
;  Retorna a interseção entre uma quantidade arbitatria de conjuntos
; (intersecao-lc conjunto? ... ) -> conjunto
(define/contract (intersecao-lc . args) (-> conjunto? ... conjunto?)
  (cond [(null? args) '()]

        [(null? (rest args))
         (first args)]

        [else (apply intersecao-lc
                     (intersecao (first args)
                                 (first (rest args))
                                 )
                     (rest (rest args))                
                     )]
    )
  )

; (uniao-lc ...)
;  Retorna a união entre uma quantidade arbitatria de conjuntos
; (uniao-lc conjunto? ... ) -> conjunto
(define/contract (uniao-lc . args) (-> conjunto? ... conjunto?)
  (cond [(null? args) '()]

        [(null? (rest args))
         (first args)]

        [else (apply uniao-lc
                     (uniao (first args)
                            (first (rest args))
                            )
                     (rest (rest args))
                     )]
        )
  )

; (dif-sim-lc ...)
;  Retorna a diferença simeterica entre uma quantidade arbitatria de conjuntos
; (dif-sim-lc conjunto? ... ) -> conjunto
(define/contract (dif-sim-lc . args) (-> conjunto? ... conjunto?)
  (cond [(null? args) '()]

        [(null? (rest args))
         (first args)]

        [else (apply dif-sim-lc
                     (dif-sim (first args)
                              (first (rest args))
                              )
                     (rest (rest args))             
                     )]
        )
  )

; (in-list-list? e list)
;  Checa se um elemento está dentro de alguma das listas que estão dentro de um lista
; (in-list-list? atom? list? ) -> boolean
(define/contract (in-list-list? e list) (-> any? list? boolean?)
  (cond [(null? list) #f]

        [(in e (first list)) #t]

        [else (in-list-list? e (rest list))]
        )
  )

; (remove-all-list-list e list)
;  Remove um elemento está dentro das listas que estão dentro de um lista
; (remove-all-list-list atom? list? ) -> list
(define/contract (remove-all-list-list e list) (-> any? list? list)
  (cond [(null? list) '()]

        [else (cons (remove e (first list))
                    (remove-all-list-list e (rest list))
                    )]
        )
  )

; (ocorre-uma-vez-lc ...)
;  Checa se um elemento ocorre apenas um vez dentre todos os conjuntos que estão em um lista
; (ocorre-uma-vez-lc conjunto? ... ) -> conjunto
(define/contract (ocorre-uma-vez-lc . args) (-> conjunto? ... conjunto?)
  (cond [(null? args) '()]

        [(null? (rest args))
         (first args)]

        [(null? (first args))
         (apply ocorre-uma-vez-lc (rest args))]

        [(not (in-list-list? (first(first args))
                             (rest args)
                             )
              )
         (cons (first (first args))
               (apply ocorre-uma-vez-lc
                      (rest (first args))
                      (rest args)                             
                      )
               )]

        [else (apply ocorre-uma-vez-lc
                     (remove-all-list-list (first(first args)) args)
                     )]
        )
  )



    