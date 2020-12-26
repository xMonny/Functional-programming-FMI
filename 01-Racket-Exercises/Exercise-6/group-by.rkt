#lang racket

; group-by
; разбива списъка xs спрямо функцията f
; например:
; (group-by even? '(1 2 3 4 5)) -> ((#f (1 3 5))
;                                   (#t (2 4)))
;(group-by length '((1 2 3) (4) (5 6 7))) -> '((1 ((4)))
;                                              (3 ((1 2 3) (5 6 7))))

; идеално би било това да стане без рекурсия

(define (group-by f xs)
  (define criteria (remove-duplicates (map (lambda (x) (f x)) xs)))
  
  (map
   (lambda (c) (cons c (list (filter (lambda (y) (equal? (f y) c)) xs))))
   criteria
  )
)