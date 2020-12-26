#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (accumulate op null-value start end term next)
  (if (= start end)
      null-value
      (op (term null-value) (accumulate op null-value (next start) end term next))))

(define (id x) x)
(define (1+ x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

; Искаме да изразим прилагане на функция n пъти чрез accumulate.
; Как изглеждаше това на упражнение?

(define (repeated f n)
  (if (= n 1) f
      (compose f (repeated f (- n 1)))))

(define (repeat f n)
  (accumulate compose f 1 n id 1+)
)

((repeated (lambda (x) (+ x 1)) 10) 5)
((repeat (lambda (x) (+ x 1)) 10) 5)

(define tests
  (test-suite "Repeat tests"
      (check-equal? ((repeat (lambda (x) (+ x 1)) 3) 5) 8)
      ; Искаме да проверим дали нашата accumulate версия прави същото като това, което написахме на упражнение
      (let ((f (lambda (x) (expt x 2)))
            (arg 2))
        (check-equal? ((repeat f 2) arg) ((repeated f 2) arg)))
  )
)

(run-tests tests 'verbose)