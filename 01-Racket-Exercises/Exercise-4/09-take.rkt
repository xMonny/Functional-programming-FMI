#lang racket

#|(define (take n xs)
  (define (loop li i n)
    (if (= i n)
        (car li)
        (cons (car li) (cons (loop (cdr li) (+ i 1) n) '()))
    )
  )

  (cond
    ((= n 0) '())
    ((null? xs) '())
    (else (loop xs 1 n))
  )
)|#

(define (take n xs)
  (cond
    ((or (= n 0) (null? xs)) '())
    (else (cons (car xs) (take (- n 1) (cdr xs))))
  )
)

(define (takewhile p? xs)
  (cond
    ((or (null? xs) (not (p? (car xs)))) '())
    (else (cons (car xs) (takewhile p? (cdr xs))))
  )
)

(takewhile even? '(2 4 6 8 9 10 11 12))
(takewhile even? '(3 5 2 4 6 8 9 10 11 12))

(take 1 '(1 2 3))