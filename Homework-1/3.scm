(#%require schemeunit)
(#%require racket/string)

(define (1+ x) (+ x 1))

(define (char-digit? c)
  (and (char>=? c #\0) (char<=? c #\9))
)

(define (char-operation? c)
  (or (char=? c #\+) (char=? c #\-) (char=? c #\*) (char=? c #\/) (char=? c #\^))
)

(define (char-space? c)
  (char=? c #\space)
)

(define (char-comma? c)
  (char=? c #\,)
)

(define (char-null? c)
  (char=? c #\null)
)

(define (case-of c)
  (case c
      ((#\+ #\-) 0)
      ((#\* #\/)  1)
      (else 2)
   )
)

(define (expr-valid? expr)
  (define (trim-spaces-from str)
    (string-normalize-spaces str)
  )

  (define (char-valid? c)
    (or (char-digit? c) (char-operation? c) (char-space? c))
  )
  
  (define (loop str i len previous)
    (cond
      ((and (= i len) (not (char-digit? previous))) #f) ;[case: "1 + 3 * 2+"]
      ((= i len) #t)
      ((and (char-null? previous) (char-operation? (get-char str i))) #f) ;[case: "^ 1*2^3"]
      ((not (char-valid? (get-char str i))) #f) ; [not digit, operation or space]
      ((and (char-space? (get-char str i)) (char-digit? previous) (char-digit? (get-char str (1+ i)))) #f) ;[case:"1+1 5 -9"]
      ((and (char-space? (get-char str i)) (char-operation? previous) (char-operation? (get-char str (1+ i)))) #f) ;[case:"1-2 + +1*9"]
      ((and (char-operation? (get-char str i)) (char-operation? previous)) #f) ;[case: 1+4-^1]
      (else (loop str (1+ i) len (get-char str i)))
    )
  )
  (let*
      (
        (new-expr (trim-spaces-from expr))
        (new-expr-len (string-length new-expr))
      )
      (if (is-empty? new-expr)
          #t
          (loop new-expr 0 new-expr-len #\null)
      )
  )
)

(define (get-last-index str)
  (if (is-empty? str)
      -1
      (- (string-length str) 1)
  )
)

(define (get-first-char str)
  (if (is-empty? str)
      #\null
      (string-ref str 0)
  )
)

(define (get-char str pos)
  (cond
    ((or (is-empty? str) (< pos 0) (>= pos (string-length str))) #\null)
    (else (string-ref str pos))
  )
)

(define (get-last-char str)
  (cond
    ((or (is-empty? str) (= (get-last-index str) -1)) #\null)
    (else (string-ref str (get-last-index str)))
  )
)

(define (is-empty? str)
  (= (string-length str) 0)
)

(define (get-first-elem str)
  (if (is-empty? str)
      ""
      (string (get-char str 0))
  )
)

(define (get-elem str pos)
  (cond
    ((or (is-empty? str) (< pos 0) (>= pos (string-length str))) "")
    (else (string (get-char str pos)))
  )
)

(define (get-last-elem str)
  (if (is-empty? str)
      ""
      (string (get-char str (get-last-index str)))
  )
)

(define (pop-back str from to)
   (if (is-empty? str)
      str
      (let*
       (
         (pref-sub (substring str from to))
       )
       (string-replace str str pref-sub)
      )
    ) 
)

(define (remove-first-char str)
  (pop-back str 1 (string-length str))
)

(define (remove-last-char str)
  (pop-back str 0 (get-last-index str))
)

(define (expr-rp expr)
  (define (remove-spaces-from str)
    (string-replace str " " "")
  )
  
  (define (loop str i len op-str ans-str sym)
    (let*
      (
        (curr-char (get-char str i))
        (curr-elem (get-elem str i))
        (op-last (get-last-char op-str))
        (erased-op-str (remove-last-char op-str))
        (ans-append-op (string-append ans-str (get-last-elem op-str)))
        (ans-append-digit (string-append ans-str curr-elem))
        (ans-append-sym (string-append ans-str sym))
        (op-append-op (string-append op-str curr-elem))
      )
      (cond
        ((and (= i len) (not (is-empty? op-str))) (loop str i len erased-op-str ans-append-op ""))
        ((= i len) ans-str)
        ((and (char-operation? curr-char) (is-empty? op-str))
         (loop str (1+ i) len op-append-op ans-append-sym sym))
        ((and (char-operation? curr-char) (> (case-of curr-char) (case-of op-last)))
         (loop str (1+ i) len op-append-op ans-append-sym sym))
        ((and (char-operation? curr-char) (<= (case-of curr-char) (case-of op-last)))
         (loop str i len erased-op-str ans-append-op ""))
        (else (loop str (1+ i) len op-str ans-append-digit ","))
      )
    )
  )
  
  (if (not (expr-valid? expr))
      #f
      (let*
         (
           (clean-expr (remove-spaces-from expr))
           (clean-expr-len (string-length clean-expr))
          )
          (if (is-empty? clean-expr)
             ""
             (loop clean-expr 0 clean-expr-len "" "" "")
          )
       )
  )
)

(define (make-operation op a b)
  (case op
    ((#\+) (+ a b))
    ((#\-) (- a b))
    ((#\*) (* a b))
    ((#\/) (/ a b))
    ((#\^) (expt a b))
    (else 0)
  )
)

(define (delete-last-number str i n ans)
  (let*
      (
        (curr-char (get-last-char str))
        (curr-elem (get-last-elem str))
        (erased-str (remove-last-char str))
        (is-end? (or (char-comma? curr-char) (is-empty? str)))
      )
      (cond
       ((and (char? ans) (eq? is-end? #t) erased-str))
       ((eq? is-end? #t) (string->number ans))
       ((char? ans) (delete-last-number erased-str (- i 1) n ans))
       (else (delete-last-number erased-str (- i 1) n (string-append curr-elem ans)))
      )
    )
)

(define (take-last-number str)
  (if (is-empty? str)
      0
      (let*
          (
            (last-index (get-last-index str))
          )
          (delete-last-number str last-index 0 "")
      )
  )
)

(define (erase-last-number str)
  (if (is-empty? str)
      ""
      (let*
          (
            (last-index (get-last-index str))
          )
          (delete-last-number str last-index 0 #\null)
      )
  )
)

(define (take-before-last-number str)
  (if (is-empty? str)
      0
      (take-last-number (erase-last-number str))
  )
)

(define (add-number str number)
  (let*
    (
      (string-number (number->string number))
    )
    (cond
      ((is-empty? str) (string-append str string-number))
      ((char-comma? (get-last-char str)) (string-append str string-number))
      (else (string-append str "," string-number))
    )
  )
)

(define (expr-eval expr)
  (define (loop str i len save-str sym)
    (let*
      (
        (curr-element (get-elem str i))
        (curr-char (get-char str i))
        (append-save-str (string-append save-str sym curr-element))
        (before-last-number (take-before-last-number save-str))
        (last-number (take-last-number save-str))
        (curr-result (make-operation curr-char before-last-number last-number))
        (double-erased-save-str (erase-last-number (erase-last-number save-str)))
      )
      (cond
        ((= i (- len 1)) curr-result)
        ((or (char-digit? curr-char) (char-comma? curr-char)) (loop str (1+ i) len append-save-str ""))
        (else (loop str (1+ i) len (add-number double-erased-save-str curr-result) ","))
      )
    )    
  )
  (cond
    ((not (expr-valid? expr)) #f)
    ((eq? (expr-rp expr) "") 0)
    (else (loop (expr-rp expr) 0 (string-length (expr-rp expr)) "" ""))
  )
)
;testing functions
(check-equal? (expr-valid? "  10 + 5       *2") #t)
(check-equal? (expr-valid? "10 20 + 5") #f)
(check-equal? (expr-valid? "-  5 + 2") #f)
(check-equal? (expr-valid? "5     + * 9 - 10") #f)
(check-equal? (expr-rp "5/5^2+1*3/2^2^3-1-5") "5,5,2^/1,3*2,2^3^/+1-5-")
(check-equal? (expr-rp "10*8^3/2+4+2^5*2-2+4-3/9") "10,8,3^*2/4+2,5^2*+2-4+3,9/-")
(check-equal? (expr-rp "10+20*30*10/5+1+2+3+4*3") "10,20,30*10*5/+1+2+3+4,3*+")
(check-equal? (expr-rp "10+10^10*5+6") "10,10,10^5*+6+")
(check-equal? (expr-rp "2^3*2") "2,3^2*")
(check-equal? (expr-rp "5/2^4") "5,2,4^/")
(check-equal? (expr-eval "10+20*30^2") 18010)
(check-equal? (expr-eval "100 * 3^7 - 90 - 8 * 3 * 2 * 9 / 4 / 6 / 2^4") 218608 7/8)
(check-equal? (expr-eval "153   /  9/2^2") 4 1/4)
(check-equal? (expr-eval "2^3^4-4096   + 100  * 5/2  /3") 83 1/3)
(check-equal? (expr-eval "5/2/4") 5/8)
(check-equal? (expr-eval "2^3^4") 4096)
(check-equal? (expr-eval "8/4/2") 1)
(check-equal? (expr-eval "2*3^2") 18)
(check-equal? (expr-eval "2^3*2") 16)
(check-equal? (expr-eval "2049 +   321 *  9 /4 /3^5 / 2 - 9 + 2^5 / 2 / 1 / 4 /5") 2042 103/360)
