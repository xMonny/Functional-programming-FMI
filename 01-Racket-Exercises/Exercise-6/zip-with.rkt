#lang racket
(require rackunit)
(require rackunit/text-ui)

; zip-with
; като zip, но не е задължително да правим двойки, а даваме функция, която да комбинира елементите - тестовете са добър източник на примери

;Решение 1
(define (zip-with-one f xs ys)
  (define (loop f xs ys ans)
    (if (or (null? xs) (null? ys))
        ans
        (loop f (cdr xs) (cdr ys) (append ans (list (f (car xs) (car ys)))))
    )
  )
  (loop f xs ys '())
)

;Решение 2
(define (zip-with f xs ys)
  (define (make-new-list L len)
    (define (make L i len)
      (cond
        ((= i len) '())
        (else (cons (car L) (make (cdr L) (+ i 1) len)))
      )
    )
    (make L 0 len)
  )
  (let
    (
      (len-xs (length xs))
      (len-ys (length ys))
    )
    (cond
      ((= len-xs len-ys) (map f xs ys))
      ((< len-xs len-ys) (map f xs (make-new-list ys len-xs)))
      (else (map f (make-new-list xs len-ys) ys))
    )
  )
)

(define tests
  (test-suite "zip-with"
    (check-equal? (zip-with + '(1 2 3) '(4 5 6)) '(5 7 9))
    (check-equal? (zip-with * '(28 9 12) '(1 3)) '(28 27))
  )
)

(run-tests tests 'verbose)