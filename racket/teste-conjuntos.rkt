#lang racket
(require "conjuntos2.rkt")
(require rackunit/text-ui)
(require rackunit)

(define teste-da-lista
  (test-suite
   "Testes da Lista 2"

   (test-case "Questão 1"
             (check-equal? CONJUNTO-VAZIO '())
             )
   
   (test-case "Questão 2"
             (check-true (conjunto-vazio? '()))
             (check-false (conjunto-vazio? '(a)))
             (check-false (conjunto-vazio? '(a b)))
             )

   (test-case "Questão 3"
             (check-true (conjunto? '()))
             (check-true (conjunto? '(a)))
             (check-true (conjunto? '(a b)))
             (check-false (conjunto? '(a b a)))
             (check-false (conjunto? '(a b e e d)))
             )

   (test-case "Questão 4"
             (check-equal? (cardinalidade-c '()) 0)
             (check-equal? (cardinalidade-c '(a b)) 2)
             (check-equal? (cardinalidade-c '((a) b c)) 3)
             )
   
   (test-case "Questão 5"
             (check-true (iguais-c? '() '()))
             (check-true (iguais-c? '(a) '(a)))
             (check-true (iguais-c? '(b a) '(a b)))
             (check-true (iguais-c? '(b (a)) '((a) b)))
             (check-false (iguais-c? '(c) '(d)))
             )

   (test-case "Questão 6"
             (check-true (subconjunto? '() '()))
             (check-true (subconjunto? '() '(a)))
             (check-true (subconjunto? '(b) '(a b)))
             (check-true (subconjunto? '(b a) '(a b)))
             (check-true (subconjunto? '(b (a)) '((a) b)))
             (check-false (subconjunto? '(a) '(b)))
             (check-false (subconjunto? '(a) '()))
             )

   (test-case "Questão 7"
             (check-false (intersecta? '() '()))
             (check-false (intersecta? '() '(a)))
             (check-true (intersecta? '(a b c) '(d e f c h)))
             (check-true (intersecta? '(b a) '(a b)))
             (check-true (intersecta? '(b (a)) '((a) b)))
             (check-false (intersecta? '(a) '(b)))
             (check-false (intersecta? '(a) '()))
             )
   
   (test-case "Questão 8"
             (check iguais-c? (intersecao '(a b c) '(b c a)) '(a b c))
             (check iguais-c? (intersecao '(a b c) '(d e c f a)) '(c a))
             (check iguais-c? (intersecao '(a b c) '()) '())
             (check iguais-c? (intersecao '() '(d e c f a)) '())
             (check iguais-c? (intersecao '() '()) '())
             )

   (test-case "Questão 9"
             (check iguais-c? (uniao '(a b c) '(b c a)) '(a b c))
             (check iguais-c? (uniao '(a b c) '(d e c f a)) '(a b c d e f))
             (check iguais-c? (uniao '(a b c) '()) '(a b c))
             (check iguais-c? (uniao '() '(d e c f a)) '(d e c f a))
             (check iguais-c? (uniao '() '()) '())
             )

   (test-case "Questão 10"
             (check iguais-c? (dif-conj '(a b c) '(b c a)) '())
             (check iguais-c? (dif-conj '(a b c) '(d e c f a)) '(b))
             (check iguais-c? (dif-conj '(a b c) '()) '(a b c))
             (check iguais-c? (dif-conj '() '(d e c f a)) '())
             (check iguais-c? (dif-conj '() '()) '())
             )

   (test-case "Questão 11"
             (check iguais-c? (dif-sim '(a b c) '(b c a)) '())
             (check iguais-c? (dif-sim '(a b c) '(d e c f a)) '(b d e f))
             (check iguais-c? (dif-sim '(a b c) '()) '(a b c))
             (check iguais-c? (dif-sim '() '(d e c f a)) '(d e c f a))
             (check iguais-c? (dif-sim '() '()) '())
             )

   (test-case "Questão 12"
             (check iguais-c? (intersecao-lc) '())
             (check iguais-c? (intersecao-lc '(a b c)) '(a b c))
             (check iguais-c? (intersecao-lc '(a b c) '(b c a)) '(a b c))
             (check iguais-c? (intersecao-lc '(a b c) '(d e c f a) '(a c)) '(a c))
             (check iguais-c? (intersecao-lc '(a b c) '(d e c f a) '(a c) '()) '())
             (check iguais-c? (intersecao-lc '() '(a b c) '(d e c f a) '(a c)) '())
             (check iguais-c? (intersecao-lc '(a b c) '(d e c f a) '(h i c j)) '(c))
             )

   (test-case "Questão 13"
             (check iguais-c? (uniao-lc) '())
             (check iguais-c? (uniao-lc '(a b c)) '(a b c))
             (check iguais-c? (uniao-lc '(a b c) '(b c a)) '(a b c))
             (check iguais-c? (uniao-lc '(a b c) '(d e c f a) '(a c)) '(a b c d e f))
             (check iguais-c? (uniao-lc '(a b c) '(d e c f a) '(a c) '()) '(a b c d e f))
             (check iguais-c? (uniao-lc '() '(a b c) '(d e c f a) '(a c)) '(a b c d e f))
             (check iguais-c? (uniao-lc '(a b c) '(d e c f a) '(h i c j)) '(a b c d e f h i j))
             )

   (test-case "Questão 14"
             (check iguais-c? (dif-sim-lc) '())
             (check iguais-c? (dif-sim-lc '(a b c)) '(a b c))
             (check iguais-c? (dif-sim-lc '(a b c) '(b c a)) '())
             (check iguais-c? (dif-sim-lc '(a b c) '(b c a) '(a c)) '(a c))
             (check iguais-c? (dif-sim-lc '(a b c) '(d e c f a) '(a c)) '(a e b d f c))
             (check iguais-c? (dif-sim-lc '(a b c) '(d e c f a) '(a c) '()) '(a e b d f c))
             (check iguais-c? (dif-sim-lc '() '(a b c) '(d e c f a) '(a c)) '(a e b d f c))
             (check iguais-c? (dif-sim-lc '(a b c) '(d e c f a) '(h i c j)) '(b d e f h i c j))
             )

   (test-case "Questão 15"
             (check iguais-c? (ocorre-uma-vez-lc) '())
             (check iguais-c? (ocorre-uma-vez-lc '(a b c)) '(a b c))
             (check iguais-c? (ocorre-uma-vez-lc '(a b c) '(b c a)) '())        
             (check iguais-c? (ocorre-uma-vez-lc '(a b c) '(d e c f a) '(a c)) '(b d e f))
             (check iguais-c? (ocorre-uma-vez-lc '() '(a b c) '(d e c f a) '(a c)) '(b d e f))
             (check iguais-c? (ocorre-uma-vez-lc '(a b c) '(d e c f a) '(h i c j)) '(b d e f h i j))
             )
   )
  )

(run-tests teste-da-lista)