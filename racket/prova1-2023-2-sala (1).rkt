#lang racket

; prova 1 de 2023.2


; Questão 1

(define (gera-sequencia n1 n2 [f identity])
  (cond
    [(= n1 n2) (list (f n1))]
    [(< n1 n2) (cons (f n1)
                     (gera-sequencia (+ n1 1) n2 f))]
    [else      (cons (f n1)
                     (gera-sequencia (- n1 1) n2 f))]
    ))

(define (gera-sequencia-v2 n1 n2 [f identity])
  (if (= n1 n2)
      (list (f n1))
      (cons (f n1) (gera-sequencia-v2 (+ n1 1) n2 f))
      ))

; Questão 2

; extrai sublista e retorna sublista e o resto
(define (e-sl e l [a '()])
  (cond
    [(null? l) (values (cons e a) '())]
    [(not (eqv? e (first l))) (values (cons e a) l)]
    [else (e-sl e (rest l) (cons (first l) a))]
    ))

(define (empacote l)
  (if (null? l)
      '()
      (let-values ([(sl rl) (e-sl (first l) (rest l))])
        (cons sl (empacote rl)))
      ))

; outra solução

(define (ti e l)
  (cond
    [(null? l) '()]
    [(not (eqv? e (first l))) '()]
    [else (cons e (ti e (rest l)))]
    ))
    
(define (td e l)
  (cond
    [(null? l) '()]
    [(not (eqv? e (first l))) l]
    [else (td e (rest l))]
    ))

(define (empacote-v2 l)
  (if (null? l)
      '()
      (cons (ti (first l) l)
            (empacote-v2 (td (first l) (rest l))))
      ))


; Questão 3
(define g1-lv '(1 2 3 4 5 6 7 8))

(define g1-la '((1 . 2) (1 . 4) (2 . 3) (4 . 3)
                        (4 . 5) (4 . 6) (3 . 7) (5 . 7) (6 . 8)))


(define (adjacentes v la-g)
  (cond
    [(null? la-g) '()]
    [(eqv? (car (first la-g)) v)
     (cons (cdr (first la-g))
           (adjacentes v (rest la-g)))]
    [(eqv? (cdr (first la-g)) v)
     (cons (car (first la-g))
           (adjacentes v (rest la-g)))]
    [else (adjacentes v (rest la-g))]
    ))

; Questão 4

(define arv '(A (B (E)) (C (F) (G) (H)) (D (I) (J))))

(define (prof-ag ag [pa 0])
  (cond
    [(null? ag) '()]
    [(not (list? (first ag)))
     (cons (cons (first ag) pa)
           (prof-ag (rest ag) pa))]
    [else
     (append (prof-ag (first ag) (+ 1 pa))
             (prof-ag (rest ag) pa))]
    ))


; Questão 5
(define (dobra-e1 f ini l)
  (if (null? l)
      ini
      (dobra-e1 f (f (first l) ini) (rest l))
      ))

(define (dobra-d1 f ini l)
  (if (null? l)
      ini
      (f (first l) (dobra-d1 f ini (rest l)))
      ))

(define (dobra-d f ini . ll)
  (if (null? (first ll))
      ini
      (apply f (append (map first ll)
                       (list (apply dobra-d f ini (map rest ll)))))
      ))

(define (dobra-e f ini . ll)
  (if (null? (first ll))
      ini
      (apply dobra-e
             f
             (apply f (append (map first ll) (list ini)))
             (map rest ll))
      ))

(define (dobra-d2 f ini pl . ll)
  (if (null? pl)
      ini
      (apply f (cons (first pl)
                     (append (map first ll)
                             (list (apply dobra-d2
                                          f
                                          ini
                                          (rest pl)
                                          (map rest ll))))))
      ))



