#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (prime? number)
  (define (prime-help? current)
    (cond
      ((= number 1) #f)
      ((> current (sqrt number)) #t)
      ((= (remainder number current) 0) #f)
      (else (prime-help? (+ current 1)))
    )
  )
  (prime-help? 2)
)

(define (accumulate-filter condition? operation null-value start end term next)
  (define (loop i result)
    (cond
      ((> i end) result)
      ((condition? i) (loop (next i) (operation result (term i))))
      (else (loop (next i) result))
    )
  )
  (loop start null-value)
)

(define (id x)
  x
)

(define (inc x)
  (+ x 1)
)

(accumulate-filter even? * 1 1 5 id inc)
(accumulate-filter prime? + 0 1 10 id inc)

(prime? 17)

(define tests
  (test-suite "Accumulate-filter sum tests"
    (check-equal? (accumulate-filter even? * 1 1 5 id inc) 8)
    (check-equal? (accumulate-filter prime? + 0 1 100 id inc) 1060)
  )
)

(run-tests tests 'verbose)