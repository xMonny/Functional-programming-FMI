#lang racket
(require rackunit)
(require rackunit/text-ui)
; 1.6 - Съчинете процедура, която обръща цифрите на дадено число.
; Трябва да работи и за отрицателни числа.

(define (find-number-digits number count)
  (cond
    ((and (= number 0) (= count 0)) 1)
    ((= number 0) count)
    (else (find-number-digits (quotient number 10) (+ count 1)))
  )
)

(define (reverse-digits-help number length newNumber)
  (if (= number 0)
      newNumber
      (reverse-digits-help
       (quotient number 10)
       (- length 1)
       (+ newNumber (* (expt 10 length) (remainder number 10)))
      )
  )
)

(define (reverse-digits number)
  (define length (find-number-digits number 0))
  (reverse-digits-help number (- length 1) 0)
)

(reverse-digits 0)

(define tests
  (test-suite
  "Reverse digits tests"

    (test-case "Should reverse correctly"
      (check-equal? (reverse-digits 2134) 4312)
    )

    (test-case "Should work alright with digits"
      (let ((k (random 10)))
        (check-equal? (reverse-digits k) k))
    )

    (test-case "Should work with negative numbers"
      (check-equal? (reverse-digits -298245) -542892)
    )
  )
)

(run-tests tests)