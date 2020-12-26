#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (remove-first x xs)
   (cond
     ((null? xs) '())
     ((= x (car xs)) (cdr xs))
     (else (cons (car xs) (remove-first x (cdr xs))))
   )
)

(define (remove-first-i x xs)
  (define (loop xs ans)
    (cond
     ((null? xs) ans)
     ((= x (car xs)) (append ans (cdr xs)))
     (else (loop (cdr xs) (append ans (list (car xs)))))
   )
  )
  (loop xs '())
)

(remove-first-i 1 '(4 1 1 2 1 3 4 5))
(append '(1 2) (cdr '(3 4 5 6)))

(define (find-min xs)
  (define (loop xs min)
    (cond
      ((null? xs) min)
      ((< (car xs) min) (loop (cdr xs) (car xs)))
      (else (loop (cdr xs) min))
    )
  )
  (cond
    ((null? xs) 0)
    ((null? (cdr xs)) (car xs))
    (else (loop (cdr xs) (car xs)))
  )
)

(find-min '(4 5 -10 2 1 -90 100))

(define (selection-sort-i xs)
  (define (loop xs ans)
    (if (null? xs)
        ans
        (let
            (
              (minimal (find-min xs))
            )
            (loop (remove-first minimal xs) (append ans (list minimal)))
        )
    )
  )

  (cond
    ((null? xs) xs)
    ((null? (cdr xs)) (car xs))
    (else (loop xs '()))
  )
)

(define (selection-sort xs)
    (if (null? xs)
        '()
        (let
            (
              (minimal (find-min xs))
            )
            (cons minimal (selection-sort (remove-first minimal xs)))
        )
    )
)

(selection-sort-i '(4 5 -10 2 1 -90 100))
(selection-sort '(4 5 -10 2 1 -90 100))

(define tests
  (test-suite "Selection sort"
     (check-equal? (selection-sort '(4 5 -10 2 1 -90 100)) '(-90 -10 1 2 4 5 100))
     (check-equal? (selection-sort '(10 9 2 -10 8 3 2 -9)) '(-10 -9 2 2 3 8 9 10))
     (check-equal? (selection-sort '(9 0 2 1 3 100 1 4 3)) '(0 1 1 2 3 3 4 9 100))
     (check-equal? (selection-sort '(-1 -4 -2 -9 -10 -1 -3 -4 -2)) '(-10 -9 -4 -4 -3 -2 -2 -1 -1))
  )
)

(run-tests tests 'verbose)