#lang racket

(require "lista01.rkt")
(require rackunit/text-ui)
(require rackunit)

(define teste-da-lista
  (test-suite
   "Testes da Lista 1"

   (test-case "Questão 1"
             (check-equal? (concatenar1 '(a b c) '(d e f g h)) '(a b c d e f g h))
             (check-equal? (concatenar1 '(a b c) '()) '(a b c))
             (check-equal? (concatenar1 '() '(d e f g h)) '(d e f g h))
             (check-equal? (concatenar1 '() '()) '())
             )

   (test-case "Questão 2"
             (check-equal? (concatenarInv '(d e f g h) '(a b c) ) '(a b c d e f g h))
             (check-equal? (concatenarInv '(a b c) '()) '(a b c))
             (check-equal? (concatenarInv '() '(d e f g h)) '(d e f g h))
             (check-equal? (concatenarInv '() '()) '())
             )

   (test-case "Questão 3"
             (check-equal? (concatenar2 '((a b) (c) (d e f g))) '(a b c d e f g))
             (check-equal? (concatenar2 '((a b c))) '(a b c))
             (check-equal? (concatenar2 '()) '())
             (check-equal? (concatenar2 '(() (a b c) ()) ) '(a b c))
             (check-equal? (concatenar2 '(() (a b c) (d)) ) '(a b c d))
             )
   

   (test-case "Questão 4"
             (check-equal? (concatenar3 '(a b) '(c) '(d e f g)) '(a b c d e f g))
             (check-equal? (concatenar3 '(a b c)) '(a b c))
             (check-equal? (concatenar3) '())
             (check-equal? (concatenar3 '() '(a b c) '() ) '(a b c))
             (check-equal? (concatenar3 '() '(a b c) '(d)) '(a b c d))
             )

   (test-case "Questão 5"
             (check-equal? (juntar '(x y z) '(a b)) '(x a y b z))
             (check-equal? (juntar '(x y) '(a b c)) '(x a y b c))
             (check-equal? (juntar '() '(a b)) '(a b))
             (check-equal? (juntar '(x y z) '()) '(x y z))
             (check-equal? (juntar '(x y z) '(a b c)) '(x a y b z c))
             )
   (test-case "Questão 6"
             (check-equal? (intercala 2 'x 'y) '(x y))
             (check-equal? (intercala 0 'x 'y) '())
             (check-equal? (intercala 1 'x 'y) '(x))
             (check-equal? (intercala 4 'x 'y) '(x y x y))
             (check-equal? (intercala 5 'x 'y) '(x y x y x))
             )

   (test-case "Questão 7"
             (check-equal? (intercala2 2 'x 'y 'z) '(x y))
             (check-equal? (intercala2 0 'x 'y 'z) '())
             (check-equal? (intercala2 1 'x 'y 'z 'w) '(x))
             (check-equal? (intercala2 4 'x 'y 'z 'w) '(x y z w))
             (check-equal? (intercala2 5 'x 'y 'z) '(x y z x y))
             (check-equal? (intercala2 9 'x 'y 'z 'w) '(x y z w x y z w x))
             )

   (test-case "Questão 8"
             (check-equal? (parear 1 '(a b c)) '((1 a) (1 b) (1 c)))
             (check-equal? (parear 'x '()) '())
             (check-equal? (parear 'x '(y)) '((x y)))
             )

   (test-case "Questão 9"
             (check-equal? (pares '(a b c d)) '((a b)
                                                (a c)
                                                (a d)
                                                (b c)
                                                (b d)
                                                (c d)))
             (check-equal? (pares '(a b)) '((a b)))
             )

   (test-case "Questão 10"
             (check-true (conjunto? '(d e f g h)))
             (check-false (conjunto? '(d e f g e h)))
             (check-true (conjunto? '()))
             )

   (test-case "Questão 11"
             (check-false (prefixo? '(a b c) '(a b)))
             (check-true (prefixo? '(a b c) '(a b c)))
             (check-true (prefixo? '(a b c) '(a b c d)))
             (check-false (prefixo? '(a b c) '(a b d c d)))
             (check-false (prefixo? '(a b c) '(a a b c)))
             )

   (test-case "Questão 12"
             (check-false (subsequencia? '(a b c) '(a b)))
             (check-true (subsequencia? '(a b c) '(a b c)))
             (check-true (subsequencia? '(a b c) '(a b c d)))
             (check-true (subsequencia? '(a b c) '(d a b c e)))
             (check-false (subsequencia? '(a b c) '(d z a b f c g)))
             )

   (test-case "Questão 13"
             (check-true (iguais-lg? '(a (b c)) '(a (b c))))
             (check-true (iguais-lg? '(a b c) '(a b c)))
             (check-false (iguais-lg? '() '(a b d)))
             (check-false (iguais-lg? '(a (b) c) '()))
             (check-true (iguais-lg? '(a (((b))) c) '(a (((b))) c) ))
             (check-true (iguais-lg? '((((b))) c) '((((b))) c) ))
             )

   (test-case "Questão 14"
             (check-equal? (substitui-lg 'c 'manoel '())  '())
             (check-equal? (substitui-lg 'c 'manoel '(a b c))  '(a b manoel))
             (check-equal? (substitui-lg 'c 'manoel '(a (b z)))  '(a (b z)))
             (check-equal? (substitui-lg 'b 'mmm '((((b))) c)) '((((mmm))) c))
             (check-equal? (substitui-lg 'c 'manoel '(a (b c)))  '(a (b manoel)))
             )

   (test-case "Questão 15"
             (check-equal? (aplanar '())  '())
             (check-equal? (aplanar '(a b c))  '(a b c))
             (check-equal? (aplanar  '(a (b c)))  '(a b c))
             (check-equal? (aplanar '((((b))) c)) '(b c))
             (check-equal? (aplanar '(() () (()))) '())
             )
   )
  )

(run-tests teste-da-lista)
