#lang racket
(require rackunit)
(require rackunit/text-ui)
; 1.7 - Търсим процедура, която проверява дали едно число е палиндром.
; Трябва да работи и за отрицателни числа.

(define (find-number-digits number count)
  (cond
    ((and (= number 0) (= count 0) 1))
    ((= number 0) count)
    (else (find-number-digits (quotient number 10) (+ count 1)))
   )
)

(define (reverse-number-help number length newNumber)
  (if (= number 0)
      newNumber
      (reverse-number-help
       (quotient number 10)
       (- length 1)
       (+ newNumber (* (expt 10 length) (remainder number 10)))
      )
  )
)

(define (reverse-number number)
  (define length (find-number-digits number 0))
  (reverse-number-help number (- length 1) 0)
)

(define (palindrome? number)
  (define reversedNumber (reverse-number number))
  ;(if (= number reversedNumber)
      ;#t
      ;#f
   ;)
  (= number reversedNumber) ;better solution
)

(palindrome? 2222)

(define tests (test-suite
  "Palindrome tests"

  (test-case "Should function correctly"
    (check-true (palindrome? 12321))
    (check-false (palindrome? 872))
    (check-true (palindrome? 2))
    (check-true (palindrome? 310013))
    (check-true (palindrome? -21212))
)))

(run-tests tests 'verbose)