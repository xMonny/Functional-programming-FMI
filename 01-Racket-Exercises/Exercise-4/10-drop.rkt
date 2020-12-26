#lang racket
(require rackunit)
(require rackunit/text-ui)

; Търсим функция, която връща списък от всички без първите n елемента на даден такъв.

(define (drop n xs)
  (define (loop xs i n ans)
    (cond
      ((null? xs) ans)
      ((> i n) (loop (cdr xs) (+ i 1) n (append ans (list (car xs)))))
      (else (loop (cdr xs) (+ i 1) n ans))
    )
  )

  (loop xs 1 n '())
)

(drop 3 '(1 2 3))

(append '() (list (car '(1 2 3))))
(define tests
  (test-suite "Take tests"
     (check-equal? (drop 2 '(1 2 3 4)) '(3 4))
     (check-equal? (drop 0 '(2 9 2)) '(2 9 2))
     (check-equal? (drop 2134 '(9 7 2 3)) '())
  )
)

(run-tests tests 'verbose)