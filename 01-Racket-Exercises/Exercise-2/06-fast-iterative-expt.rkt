#lang racket
(require rackunit)
(require rackunit/text-ui)

; Стъпвайки на дефиницията за бързо повдигане на степен,
; търсим такава, която генерира итеративен процес
(define (expt x n)
  (define (expt-helper x n result)
    (if (= n 0)
        result
        (expt-helper x (- n 1) (* result x))
    )
  )
  (expt-helper x n 1)
)

(expt 2 0)

(define tests
  (test-suite "expt tests"
    (check-equal? (expt 4 4) 256)
    (check-equal? (expt 29139123 0) 1)
    (check-equal? (expt 3 4) 81)
    (check-equal? (expt 2 1) 2)
  )
)

(run-tests tests 'verbose)