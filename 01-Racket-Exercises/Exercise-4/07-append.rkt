#lang racket
(require rackunit)
(require rackunit/text-ui)

; Търсим функция, която конкатенира два списъка

(define (append xs ys)
  (cond
    ((and (null? xs) (null? ys)) '())
    ((not (null? xs)) (cons (car xs) (append (cdr xs) ys)))
    (else (cons (car ys) (append xs (cdr ys))))
  )
)

(append '(1) '(()))
(append '(1 ((2 3))) '(1 10 (4) (10 11)))

(define tests
  (test-suite "append tests"
    (check-equal? (append '(5 9 2) '(1)) '(5 9 2 1))
    (check-equal? (append '() '(2 3)) '(2 3))
    (check-equal? (append '(2 3) '()) '(2 3))
    (check-equal? (append '(1 8 6 2 3) '(2 3)) '(1 8 6 2 3 2 3))
  )
)

(run-tests tests 'verbose)