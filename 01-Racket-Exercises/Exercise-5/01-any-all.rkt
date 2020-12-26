#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме функция, която ни казва дали поне един елемент в даден списък
; изпълнява някакво условие

(define (any? p? xs)
  (cond
    ((null? xs) #f)
    ((p? (car xs)) #t)
    (else (any? p? (cdr xs)))
  )
)

(any? even? '(2 4 4 2 8 10 2 9))

#|(define (all? p? xs)
  (cond
    ((null? xs) #t)
    ((not (p? (car xs))) #f)
    (else (all? p? (cdr xs)))
  )
)

(all? even? '(21 4 4 2 8 10 2 14))

(define (remove xs index)
  (define (loop xs i ans)
    (cond
      ((null? xs) ans)
      ((= i index) (append ans (cdr xs)))
      (else (loop (cdr xs) (+ i 1) (append ans (list (car xs)))))
    )
  )
  (loop xs 0 '())
)

(remove '(1 2 3 4) 2)|#

(define (all? p? xs)
  (cond
    ((null? xs) #t)
    ((any? p? xs) (all? p? (cdr xs)))
    (else #f)
  )
)

(all? even? '(21 4 4 2 8 10 2 14))

(define any?-tests
  (test-suite ""
    (check-true (any? odd? '(2 4 4 2 8 9 2 0)))
    (check-true (any? (lambda (x) (> (length x) 3)) '((1 2 3) (3 4 4 2) (2 1))))
    (check-false (any? (lambda (x) (> x 2)) (map (lambda (x) (remainder x 3)) (range 1 100))))
  )
)

(define all?-tests
  (test-suite ""
    (check-true (all? (lambda (x) (> x 100)) (range 101 103)))
  )
)

(run-tests any?-tests 'verbose)
(run-tests all?-tests 'verbose)