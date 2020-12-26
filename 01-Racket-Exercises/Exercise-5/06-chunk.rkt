#lang racket
(require rackunit)
(require rackunit/text-ui)

; chunk 
; разбива списъка xs на подсписъци с дължина n

(define (chunk n xs)
  (define (safe-take n xs)
    (cond
      ((> n (length xs)) xs)
      (else (take xs n))
    )
  )

  (define (safe-drop n xs)
    (cond
      ((> n (length xs)) '())
      (else (drop xs n))

    )
  )
  
  (if (null? xs)
      '()
      (cons (safe-take n xs) (chunk n (safe-drop n xs)))
  ) 
)

(define tests
  (test-suite "chunk"
    (check-equal? (chunk 2 '(1 2 3 4 5 6 7 8 9))  '((1 2) (3 4) (5 6) (7 8) (9)))
    (check-equal? (chunk 3 '(1 2 3 4 5 6 7 8 9))  '((1 2 3) (4 5 6) (7 8 9)))
  )
)

(run-tests tests 'verbose)