#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да вземем последния елемент на даден списък

(define (last xs)
  (define (get-last li curr)
    (if (null? li)
        curr
        (get-last (cdr li) (car li))
    )
  )
  (if (null? xs)
      #f
      (get-last xs (car xs))
  )
)

(last '(-1 -4 (1 2)))

(define tests
  (test-suite "List ref tests"
    (check-equal? (last '(5 9 2)) 2)
    (check-equal? (last '(1 8 6 2 3)) 3)
    (check-equal? (last '(1)) 1)
    (check-equal? (last '(2 3)) 3)
  )
)

(run-tests tests 'verbose)