#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да проверим дали число е просто.

(define (prime-helper? number checkNumber rem)
  (cond
    ((= number 0) #f)
    ((= number 1) #t)
    ((= rem 0) #f)
    ((and (= checkNumber number) (> rem 0)) #t)
    (else (prime-helper? number (+ checkNumber 1) (remainder number checkNumber)))
  )
)

(define (prime? number)
  (prime-helper? number 2 -1)
)

;(prime? 1)

(define tests
  (test-suite "prime? tests"
    (check-false (prime? 1))
    (check-true (prime? 5))
    (check-false (prime? 1729))
    (check-false (prime? 41041))
  )
)

(run-tests tests 'verbose)