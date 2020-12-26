#lang racket
(require rackunit)
(require rackunit/text-ui)

; Търсим процедура, която проверява дали дадено число завършва на дадено друго.

(define (ends-with? number test)
  (define (ends-with-help? number test rem1 rem2)
    (cond
     ((and (= number 0) (> test 0)) #f)
     ((not (= rem1 rem2)) #f)
     ((and (>= number 0) (= test 0) (= rem1 rem2)) #t)
     (else (ends-with-help? (quotient number 10) (quotient test 10) (remainder number 10) (remainder test 10)))
    )
   )

  (if (= test 0)
      (if (= (remainder number 10) 0)
          #t
          #f
       )
      (ends-with-help? number test 0 0)
  )
)

;(ends-with? 1980 0)

(define tests
  (test-suite "ends-with? tests"
    (check-true (ends-with? 8317 17))
    (check-true (ends-with? 82 82))
    (check-false (ends-with? 8213 31))
    (check-true (ends-with? 210 0))
    (check-false (ends-with? 2921 2))
    (check-false (ends-with? 213 0))
  )
)

(run-tests tests 'verbose)