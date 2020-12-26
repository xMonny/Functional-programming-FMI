#lang racket
(require rackunit)
(require rackunit/text-ui)

; Функцията sum, която видяхме на упражнение.
; Да стане по итеративен начин.

(define (id x)
  x
)

(define (inc x)
  (+ x 1)
)

(define (sum start end term next)
  (define (sum-help start end result term next)
    (if (> start end)
      result
      (sum-help (next start) end (+ result start) term next)
    )
  )
  (sum-help start end 0 term next)
)

(sum 1 10 id inc)

(define tests
  (test-suite "Iterative sum tests"

    (check-equal? (sum 1 100 id inc) 5050)
    (check-equal? (sum 9 9 id inc) 9)
  )
)

(run-tests tests 'verbose)