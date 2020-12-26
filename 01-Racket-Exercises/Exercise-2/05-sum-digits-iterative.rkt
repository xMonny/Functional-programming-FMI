#lang racket
(require rackunit)
(require rackunit/text-ui)

; Търсим сумата от цифрите на дадено число.
; Процедурата да генерира итеративен процес.
(define (sum-digits number)
  (define (sum-digits-help number result)
    (if (= number 0)
        (abs result)
        (sum-digits-help (quotient number 10) (+ result (remainder number 10)))
    )
  )
  (sum-digits-help number 0)
)

(sum-digits -78932)

(define tests
  (test-suite "to-decimal tests"
    (check-equal? (sum-digits 11001) 3)
    (check-equal? (sum-digits 804357) 27)
    (check-equal? (sum-digits 981) 18)
  )
)

(run-tests tests 'verbose)