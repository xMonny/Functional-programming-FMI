#lang racket
(require rackunit)
(require rackunit/text-ui)

; Обръщаме число в двоична бройна система

(define (to-binary number)
  (define (to-binary-help number pos res)
    (if (= number 0)
        res
        (to-binary-help (quotient number 2) (+ pos 1) (+ res (* (expt 10 pos) (remainder number 2))))
    )
  )
  (to-binary-help number 0 0)
)

(to-binary 2)

(define tests
  (test-suite "to-binary tests"
    (check-equal? (to-binary 10) 1010)
    (check-equal? (to-binary 0) 0)
    (check-equal? (to-binary 8) 1000)
  )
)

(run-tests tests 'verbose)