#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да вземем i-тия елемент от списъка lst, като броим от 0.

(define (list-ref lst i)
  (define (get-elem li curr pos elem)
    (cond
      ((> curr pos) elem)
      ((null? li) #f)
      (else (get-elem (cdr li) (+ curr 1) pos (car li)))
    )
  )
  (if (null? lst)
      #f
      (get-elem lst 0 i (car lst))
  )
)

(list-ref '(2 3) 2)

(define tests
  (test-suite "List ref tests"
    (check-equal? (list-ref '(5 9 2) 0) 5)
    (check-equal? (list-ref '(1 8 6 2 3) 4) 3)
  )
)

(run-tests tests 'verbose)