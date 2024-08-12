#lang racket
(require racket/trace)

(define l1 '(a a a a b b b c c b c a a d e e))

(define arv '(5 (3 (1 () ()) (4 () ())) (7 () (8 ()()))))

(define arv2 '(5 (3 (1 () ()) (4 () ())) (3 (4 () ()) (1 ()()))))

(define arv3 '(1 (2 (4 () ()) (5 () ())) (3 (6 () ()) (7 () ()))))

(define arv4 '(1 (2 (3 () ()) ()) (2 () (3 () ()))))

(define ag1 '(A (B (E)) (C (F) (G) (H)) (D (I) (J))))


; QUESTÃO 1

(define (gera-sequencia n1 n2 [f identity])
  (cond
    [(= n1 n2) (list (f n1))]
    [(< n1 n2) (cons (f n1) (gera-sequencia (+ n1 1) n2 f))]
    [else (cons (f n1) (gera-sequencia (- n1 1) n2 f))]
    ))



; QUESTÃO 2

(define (empacote3 l)
  (if (null? l)
      '()
      (let [(res (remove-aux3 (first l) l '() '()))]
        (cons (first res) (empacote3 (rest res))))
      ))

(define (remove-aux3 e l le ls)
  (cond
    [(null? l) (cons le ls)]
    [(eqv? e (first l))
     (remove-aux3 e (rest l) (cons e le) ls)]
    [else
     (remove-aux3 e (rest l) le (cons (first l) ls))]
    ))



; QUESTÃO 3

(define (enqueue e l)
  (if (null? l)
      (list e)
      (cons (first l) (enqueue e (rest l)))))


(define (al arv)
  (if (null? arv)
      '()
      (em-linha (list arv))))

(define (em-linha la)
  (if (null? la)
      '()
      (let [(arv (first la))]
        (if (null? arv)
            (em-linha (rest la))
            (cons (first arv)
                  (em-linha 
                   (enqueue (third arv)
                            (enqueue (second arv)
                                     (rest la)))))))
      ))
  


(define (al2 arv)
  (if (null? arv)
      '()
      (em-linha2 (list arv))))

(define (em-linha2 la)
  (if (null? la)
      '()
      (let [(arv (first la))]
        (if (null? arv)
            (cons 'nil (em-linha2 (rest la)))
            (cons (first arv)
                  (em-linha2 
                   (enqueue (third arv)
                            (enqueue (second arv)
                                     (rest la)))))))
      ))
  

;QUESTÃO 4

; versão 1
(define (grau-ag ag)
  (if (null? ag)
      '()
      (cons (cons (first ag) (length (rest ag)))
            (apply append (map grau-ag (rest ag))) 
            )))


; versão 2 - sem o map e o apply
(define (grau-ag2 ag)
  (cond
    [(null? ag)'()]
    [(list? (first ag))
       (append (grau-ag2 (first ag))
               (grau-ag2 (rest ag)))]
    [else
      (cons (cons (first ag) (length (rest ag)))
            (grau-ag2 (rest ag)))] 
            ))

  
; 
;QUESTÃO 5


(define (dobra-d f ini . ll)
  (if (null? (first ll))
      ini
      (apply f (append (map first ll)
                       (list (apply dobra-d f ini (map rest ll)))))
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




