#lang racket
(require rackunit)
(require rackunit/text-ui)


; Обръщаме число от двоична в десетична бройна система
(define (to-decimal number)
  (define (to-decimal-help number pos res)
    (if (= number 0)
        res
        (to-decimal-help (quotient number 10) (+ pos 1) (+ res (* (expt 2 pos) (remainder number 10))))
    )
  )
  (to-decimal-help number 0 0)
)

(to-decimal 1)

(define tests
  (test-suite "to-decimal tests"
    (check-equal? (to-decimal 11001) 25)
    (check-equal? (to-decimal 1100011) 99)
  )
)

(run-tests tests 'verbose)
