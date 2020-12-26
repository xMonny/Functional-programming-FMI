#lang racket
(require rackunit)
(require rackunit/text-ui)

; Търсим функция, която обръща даден списък
(define (reverse xs)
  (define (loop xs)
    (if (null? (cdr xs))
      (list (car xs))
      (append (loop (cdr xs)) (list (car xs)))
     )
  )

  (cond
    ((or (null? xs) (null? (cdr xs))) xs)
    (else (loop xs))
  )
)

(cons 1 (cons 2 '()))
(cons (car '(1 5)) (cons (car '(3 2)) '()) )
(cdr '(5))
(cdr '(1 5))
(reverse '(1 5))
(reverse '(1 5 9))
(reverse '())

; И нейн итеративен вариант
(define (reverse-iter xs)
  (define (loop xs ans)
    (if (null? xs)
       ans
       (loop (cdr xs) (append (list (car xs)) ans))
    )
  )
  (cond
    ((or (null? xs) (null? (cdr xs))) xs)
    (else (loop xs '()))
  )
)

(reverse-iter '(1 5 9))
(reverse-iter '(1 2 3))
(reverse '(1 2 3))
;(append '(2) (list 1))

(define tests
  (test-suite "Reverse tests"
      (check-equal? (reverse-iter '(1 2 3)) (reverse '(1 2 3)))
      (check-equal? (reverse '()) '())
      (check-equal? (reverse '(1)) '(1))
      (check-equal? (reverse '(1 5)) '(5 1))
  )
)

(run-tests tests 'verbose)