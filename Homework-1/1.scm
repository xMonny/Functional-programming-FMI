(define (accumulate op term init a next b)  
  (define (loop i)
      (if (< i b)
          (op (term i) (loop (next i)))
          init
      )
  )
  (loop a)
)

(define (squares n)
  (define columns (- (* 4 n) 1))
  (define iterations-upper n)
  (define iterations-lower (- n 1))

  (define (id x) x)
  (define (next x) (+ x 1))
  (define (previous x) (- x 1))
    
  (define (left-part sign start end)
    (define (op x y) (display sign)(display #\space))
    (accumulate op id 0 start next end)
  )
  (define (middle-part sign start end)
    (define (op x y) (display sign))
    (accumulate op id 0 start next end)
  )
  (define (right-part sign start end)
    (define (op x y) (display #\space)(display sign))
    (accumulate op id 0 start next end)
  )

  (define (loop comp i op-i end-i s1 s2)
    (if (comp i end-i)
        (begin
          (left-part #\│ 0 i)
          (middle-part s1 0 1)
          (middle-part #\─ 1 (- columns (* 4 i) 1))
          (middle-part s2 0 1)
          (right-part #\│ 0 i)
          (display #\newline)
          (loop comp (op-i i) op-i end-i s1 s2)
        )
        (display "")
    )
  )

  (begin
    (loop < 0 next iterations-upper #\┌ #\┐)
    (loop >= iterations-lower previous 0 #\└ #\┘)
  )
)

(squares 15)