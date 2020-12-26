(#%require schemeunit)

(define (next x) (+ x 1))

(define (convert set from to op next)
  (define (loop set i result)
    (if (= set 0)
        result
        (loop (quotient set to) (next i) (op result (* (remainder set to) (expt from i)))) 
    )
  )
  (loop set 0 0)
)

(define (convert-to-binary set)
  (convert set 10 2 + next)
)

(define (convert-to-decimal set)
  (convert set 2 10 + next)
)

(define (change set op elem)
  (let*
      (
        (set-binary (convert-to-binary set))
        (changed-set-binary (op set-binary (expt 10 elem)))
      )
     (convert-to-decimal changed-set-binary)
  )
)

(define (set-add set elem)
  (cond
    ((or (< set 0) (< elem 0)) -1)
    ((set-contains? set elem) set)
    (else (change set + elem))
  )
)

(define (set-remove set elem)
  (cond
    ((or (< set 0)(< elem 0)) -1)
    ((not (set-contains? set elem)) set)
    (else (change set - elem))
  )
)

(define (set-contains? set elem)
  (define (search-elem set i elem current)
    (cond
      ((and (> i elem) (= current 1)) #t)
      ((and (> i elem) (= current 0)) #f)
      (else (search-elem (quotient set 10) (next i) elem (remainder set 10)))
    )
  )

  (cond
    ((< set 0) #f)
    ((set-empty? set) #f)
    (else (search-elem (convert-to-binary set) 0 elem -1))
  )
)

(define (set-empty? set)
  (if (= set 0)
      #t
      #f
  )
)

(define (pass-all s1 s2 i comp curr to cond? op result)
  (cond
    ((and (= s1 0) (= curr 0)) result)
    ((and (comp curr to) (cond? s2 i)) (pass-all (quotient s1 10) s2 (next i) comp (remainder s1 10) to cond? op (op result i)))
    (else (pass-all (quotient s1 10) s2 (next i) comp (remainder s1 10) to cond? op result))
  )
)

(define (set-size set)
  (define (next result i) (+ result 1))
  (define (return-true? set elem) #t)
  (cond
    ((< set 0) -1)
    ((set-empty? set) 0)
    (else (pass-all (convert-to-binary set) 0 0 = -1 1 return-true? next 0))
  )
)

(define (merge s1 op s2 comp to)    
  (define (sum result i) (+ result (expt 10 i)))
  (define (return-true? set elem) #t)
  (let*
      (
        (s1-binary (convert-to-binary s1))
        (s2-binary (convert-to-binary s2))
        (merged-binary (op s1-binary s2-binary))
      )
      (if (= s1 s2)
        s1
        (convert-to-decimal (pass-all merged-binary 0 -1 comp -1 to return-true? sum 0))
      )
    )
)

(define (set-intersect s1 s2)   
  (cond
    ((or (< s1 0) (< s2 0)) -1)       
    ((or (set-empty? s1) (set-empty? s2)) 0)
    (else (merge s1 + s2 = 2))
  )
)

(define (set-union s1 s2)
  (cond
    ((or (< s1 0) (< s2 0)) -1)       
    ((set-empty? s1) s2)
    ((set-empty? s2) s1)
    (else (merge s1 + s2 > 0))
  )
)

(define (set-difference s1 s2)                    
  (define (sum result i) (+ result (expt 10 i)))
  (define (not-contains? set elem) (not (set-contains? set elem)))
  (cond
    ((or (< s1 0) (< s2 0)) -1)       
    ((set-empty? s1) 0)
    ((set-empty? s2) s1)
    (else (convert-to-decimal (pass-all (convert-to-binary s1) s2 -1 = -1 1 not-contains? sum 0)))
  )
)

(define (take-price set p)
  (define (loop set i current result)
    (cond
      ((= current 1) (loop (quotient set 10) (next i) (remainder set 10) (+ result (p i))))
      ((= set 0) result)
      (else (loop (quotient set 10) (next i) (remainder set 10) result))
    )
  )
  (define set-binary (convert-to-binary set))
  (loop set-binary -1 -1 0)
)

(define (knapsack c n w p)
  (define (loop c n w p set)
    (cond
      ((or (= n 0) (= c 0)) set)
      ((> (w (- n 1)) c) (loop c (- n 1) w p set))
      (else
        (let* (
               (first (+ (p (- n 1)) (take-price (loop (- c (w (- n 1))) (- n 1) w p (set-add set (- n 1))) p)))
               (second (take-price (loop c (- n 1) w p set) p))
              )
              (if (> first second)
                  (loop (- c (w (- n 1))) (- n 1) w p (set-add set (- n 1)))
                  (loop c (- n 1) w p set)
              )
        )
      )
    )
  )
  (loop c n w p 0)
)

;testing functions
(check-equal? (convert-to-binary 4) 100)
(check-equal? (convert-to-binary 390084608) 10111010000000011100000000000)
(check-equal? (convert-to-binary 12191744) 101110100000100000000000)
(check-equal? (convert-to-binary 391814306) 10111010110101001110010100010)

(check-equal? (convert-to-decimal 100) 4)
(check-equal? (convert-to-decimal 10111010000000011100000000000) 390084608)
(check-equal? (convert-to-decimal 101110100000100000000000) 12191744)
(check-equal? (convert-to-decimal 10111010110101001110010100010) 391814306)

(check-equal? (set-add 4 0) 5)
(check-equal? (set-add 8 10) 1032)
(check-equal? (set-add 137 3) 137)
(check-equal? (set-add 137 6) 201)

(check-equal? (set-remove 4 0) 4)
(check-equal? (set-remove 8 10) 8)
(check-equal? (set-remove 137 3) 129)
(check-equal? (set-remove 137 7) 9)

(check-equal? (set-contains? 4 3) #f)
(check-equal? (set-contains? 4 2) #t)
(check-equal? (set-contains? 5 0) #t)
(check-equal? (set-contains? 1109 4) #t)
(check-equal? (set-contains? 1109 9) #f)

(check-equal? (set-empty? 0) #t)
(check-equal? (set-empty? 5) #f)
(check-equal? (set-empty? 110) #f)
(check-equal? (set-empty? 1190) #f)

(check-equal? (set-size 0) 0)
(check-equal? (set-size 5) 2)
(check-equal? (set-size 110) 5)
(check-equal? (set-size 1109) 5)
(check-equal? (set-size 4546031) 15)

(check-equal? (set-intersect 110 444) 44) 
(check-equal? (set-intersect 4546031 109823) 68847)
(check-equal? (set-intersect 351685 5495) 5445)
(check-equal? (set-intersect 1393981 391478580) 1393972)
(check-equal? (set-intersect 391478580 1393981) 1393972)

(check-equal? (set-union 5495 351685) 351735)
(check-equal? (set-union 4546031 109823) 4587007)
(check-equal? (set-union 1393981 391478580) 391478589)
(check-equal? (set-union 391478580 1393981) 391478589)

(check-equal? (set-difference 5495 351685) 50)
(check-equal? (set-difference 4546031 109823) 4477184)
(check-equal? (set-difference 351685 5495) 346240)
(check-equal? (set-difference 1393981 391478580) 9)
(check-equal? (set-difference 391478580 1393981) 390084608)

(define (w1 i)
  (case i
    ((0) 12)
    ((1) 1)
    ((2) 4)
    ((3) 1)
    ((4) 2)
  )
)

(define (p1 i)
  (case i
    ((0) 4)
    ((1) 2)
    ((2) 10)
    ((3) 1)
    ((4) 2)
  )
)

(define (w2 i)
  (case i
    ((0) 3)
    ((1) 1)
    ((2) 4)
  )
)

(define (p2 i)
  (case i
    ((0) 2)
    ((1) 5)
    ((2) 1)
  )
)

(define (w3 i)
  (case i
    ((0) 1)
    ((1) 5)
    ((2) 2)
  )
)

(define (p3 i)
  (case i
    ((0) 1)
    ((1) 1)
    ((2) 1)
  )
)

(define (w4 i)
  (case i
    ((0) 30)
    ((1) 10)
    ((2) 20)
  )
)

(define (p4 i)
  (case i
    ((0) 120)
    ((1) 60)
    ((2) 100)
  )
)

(define (w5 i)
  (case i
    ((0) 10)
    ((1) 8)
    ((2) 5)
    ((3) 20)
    ((4) 1)
    ((5) 4)
    ((6) 7)
  )
)

(define (p5 i)
  (case i
    ((0) 15)
    ((1) 20)
    ((2) 11)
    ((3) 30)
    ((4) 4)
    ((5) 11)
    ((6) 8)
  )
)

(check-equal? (knapsack 15 5 w1 p1) 30)
(check-equal? (knapsack 5 3 w2 p2) 3)
(check-equal? (knapsack 5 3 w3 p3) 5)
(check-equal? (knapsack 50 3 w4 p4) 5)
(check-equal? (knapsack 5 7 w5 p5) 48)
(check-equal? (knapsack 10 7 w5 p5) 52)