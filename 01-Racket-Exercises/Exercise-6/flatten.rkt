#lang racket
(require rackunit)
(require rackunit/text-ui)

; flatten
; функция, която премахва допълнителни нива на влагане в списъци
; за да я имплементираме, е хубаво да видим какво правят функциите atom? и/или list?
; хубаво е и да помислим дали не може да използваме някоя от map/filter/fold за проблема

(define (flatten xs)
  (cond
    ((null? xs) '())
    ((not (list? (car xs))) (cons (car xs) (flatten (cdr xs))))
    (else (flatten (append (car xs) (cdr xs))))
  )
)

(flatten '((1) 2 ((3 4 (5)) (((((6))))))))

(define tests
  (test-suite "flatten"
    (check-equal? (flatten '((1) 2 ((3 4 (5)) (((((6))))))))  '(1 2 3 4 5 6))
    (check-equal? (flatten '(1 2 3 (4)))  '(1 2 3 4))
    (check-equal? (flatten '(() () 3 (2))) '(3 2))
  )
)

(run-tests tests 'verbose)