#lang racket/base
(require racket/string)
(require racket/stream)

(provide tree?)
(provide string->tree)
(provide tree-valid?)
(provide balanced?)
(provide tree->string)
(provide ordered?)
(provide stream-equal?)
(provide tree->stream)
(provide tree-height)
(provide tree-width)

(define (digit? c)
  (and (char>=? c #\0) (char<=? c #\9))
)
(define (star? c)
  (char=? c #\*)
)
(define (open-bracket? c)
  (char=? c #\{)
)
(define (close-bracket? c)
  (char=? c #\})
)
(define (space? c)
  (char=? c #\space)
)
(define (check-valid? c)
  (or (digit? c)
      (star? c)
      (open-bracket? c)
      (close-bracket? c)
      (space? c))
)

(define (is-empty? str)
  (= (string-length str) 0)
)

(define (pop-back str)
  (if (is-empty? str)
      ""
      (substring str 0 (- (string-length str) 1))
  )
)

(define (get-char str i)
  (cond
    ((or (is-empty? str) (< i 0) (>= i (string-length str))) #\null)
    (else (string-ref str i))
  )
)

(define (get-last-char str)
  (if (is-empty? str)
      #\null
      (get-char str (- (string-length str) 1))
  )
)

(define (tree? str)
  (define (syntactic-check str i len)
    (let*
       (
         (curr (get-char str i))
         (prev (get-char str (- i 1)))
         (next (get-char str (+ i 1)))
       )
      (cond
        ((= i (- len 1)) #t)
        ((not (check-valid? curr)) #f)
        ((and (space? curr) (digit? prev) (digit? next)) #f)
        ((and (space? curr) (digit? prev) (close-bracket? next)) #f)
        ((and (space? curr) (open-bracket? prev) (open-bracket? next)) #f)
        ((and (space? curr) (open-bracket? prev) (close-bracket? next)) #f)
        ((and (open-bracket? curr) (open-bracket? next)) #f)
        ((and (open-bracket? curr) (close-bracket? next)) #f)
        (else (syntactic-check str (+ i 1) len))     
      )
    )
  )

  (define (brackets-stars-check str i len help number-star)
    (let*
      (
        (curr (get-char str i))
        (help-last (get-last-char help))
      )
      (cond
        ((= i len) #t)
        ((and (close-bracket? curr) (star? help-last) (>= number-star 2)) #f)
        ((and (close-bracket? curr) (digit? help-last) (< number-star 2)) #f)
        ((and (close-bracket? curr) (open-bracket? help-last) (< number-star 2)) #f)
        ((and (close-bracket? curr) (star? help-last)) (brackets-stars-check str i len (pop-back help) (+ number-star 1)))
        ((and (close-bracket? curr) (digit? help-last)) (brackets-stars-check str i len (pop-back help) number-star))
        ((and (close-bracket? curr) (open-bracket? help-last)) (brackets-stars-check str (+ i 1) len (string-append (pop-back help) "*") 0))
        (else (brackets-stars-check str (+ i 1) len (string-append help (string curr)) 0))
      )
    )
  )

  (let*
    (
      (normal-spaces-str (string-normalize-spaces str))
      (normal-spaces-str-len (string-length normal-spaces-str))
      (first (get-char normal-spaces-str 0))
      (last (get-char normal-spaces-str (- normal-spaces-str-len 1)))
      (no-spaces-str (string-replace normal-spaces-str " " ""))
      (no-spaces-str-len (string-length no-spaces-str))
    )
    (cond
      ((string=? no-spaces-str "*") #t)
      ((is-empty? normal-spaces-str) #f)
      ((not (open-bracket? first)) #f)
      ((not (close-bracket? last)) #f)
      ((not (syntactic-check normal-spaces-str 0 normal-spaces-str-len)) #f)
      (else (brackets-stars-check no-spaces-str 0 no-spaces-str-len "" 0))
    )
  )
)

(define (add-digit-to-string str ch)
  (if (not (digit? ch))
      str
      (string-append str (string ch))
  )
)

(define (string->tree str)
  (define (push-back stack element)
     (cons (append (car stack) (list element)) (cdr stack))
  )
  
  (define (convert-to-tree str i len save stack)
    (let*
      (
        (curr (get-char str i))
        (next (get-char str (+ i 1)))
        (num (string->number (add-digit-to-string save curr)))
      )
      (cond
        ((= i (- len 1)) (car stack))                                                                           
        ((open-bracket? curr) (convert-to-tree str (+ i 1) len "" (cons '() stack)))
        ((and (digit? curr) (or (open-bracket? next) (star? next))) (convert-to-tree str (+ i 1) len "" (push-back stack num)))
        ((digit? curr) (convert-to-tree str (+ i 1) len (add-digit-to-string save curr) stack))
        ((star? curr) (convert-to-tree str (+ i 1) len "" (push-back stack '())))
        (else (convert-to-tree str (+ i 1) len "" (push-back (cdr stack) (car stack))))
      )
    )
  )

  (let*
    (
      (no-spaces-str (string-replace str " " ""))
      (len (string-length no-spaces-str))
    )
    (cond
      ((not (tree? no-spaces-str)) #f)
      ((string=? no-spaces-str "*") '())
      (else (convert-to-tree no-spaces-str 0 len "" '(())))
    )
  )
  
)

(define empty-tree '())
(define (empty-tree? tree) (if (null? tree) #t #f))
(define (root-tree tree) (if (empty-tree? tree) -1 (car tree)))
(define (left-tree tree) (if (empty-tree? tree) empty-tree (cadr tree)))
(define (right-tree tree) (if (empty-tree? tree) empty-tree (caddr tree)))

(define (tree-valid? tree)
  (cond
    ((empty-tree? tree) #t)
    ((not (list? tree)) #f)
    ((not (equal? (length tree) 3)) #f)
    ((not (number? (root-tree tree))) #f)
    (else (and (tree-valid? (left-tree tree)) (tree-valid? (right-tree tree))))
  )
)

(define (leaf? tree)
  (cond
    ((not (list? tree)) #f)
    ((empty-tree? tree) #f)
    ((not (number? (root-tree tree))) #f)
    ((null? (cdr tree)) #f)
    ((null? (cddr tree)) #f)
    ((and (empty-tree? (left-tree tree)) (empty-tree? (right-tree tree)) (null? (cdddr tree))) #t)
    (else #f)
  ) 
)

(define (has-two-leaves? tree)
  (cond
    ((not (tree-valid? tree)) #f)
    ((empty-tree? tree) #f)
    ((leaf? tree) #f)
    ((or (empty-tree? (left-tree tree)) (empty-tree? (right-tree tree))) #f)
    ((or (not (leaf? (left-tree tree))) (not (leaf? (right-tree tree)))) #f)
    (else #t)
  )
)

(define (sort tree op)
  (if (empty-tree? tree)
      empty-tree
      (op (root-tree tree) (sort (left-tree tree) op) (sort (right-tree tree) op))
  )
)
  
(define (preorder tree)
  (if (not (tree-valid? tree))
      '()
      (sort tree (lambda (N L R) (append (list N) L R)))
  )
)
(define (inorder tree)
  (if (not (tree-valid? tree))
      '()
      (sort tree (lambda (N L R) (append L (list N) R)))
  )
)
(define (postorder tree)
  (if (not (tree-valid? tree))
      '()
      (sort tree (lambda (N L R) (append L R (list N))))
  )
)

(define (sorted-list? xs)
  (define (check-sorted comp xs prev)
    (cond
      ((null? xs) #t)
      ((comp (car xs) prev) #f)
      (else (check-sorted comp (cdr xs) (car xs)))
    )
  )

  (cond
    ((null? xs) #t)
    ((null? (cdr xs)) #t)
    (else (or (check-sorted < (cdr xs) (car xs)) (check-sorted > (cdr xs) (car xs))))
  )
)

(define (ordered? tree)
  (if (not (tree-valid? tree))
      #f
      (sorted-list? (inorder tree))
  )
)

(define (tree->string tree)
  (define (convert-to-string tree)
    (if (empty-tree? tree)
       " *"
       (string-append " {" (number->string (root-tree tree)) (convert-to-string (left-tree tree)) (convert-to-string (right-tree tree)) "}")
    )
  )

  (if (not (tree-valid? tree))
      ""
      (string-normalize-spaces (convert-to-string tree))
  )
)

(define (balanced? tree)
  (define (get-height tree)
    (if (empty-tree? tree)
       0
       (+ 1 (max (get-height (left-tree tree)) (get-height (right-tree tree))))
    )
  )
  
  (define (is-balanced? tree)
    (cond
      ((empty-tree? tree) #t)
      ((> (abs(- (get-height (left-tree tree)) (get-height (right-tree tree)))) 1) #f)
      ((or (not (is-balanced? (left-tree tree))) (not (is-balanced? (right-tree tree)))) #f)
      (else #t)
    )
  )

  (if (not (tree-valid? tree))
      #f
      (is-balanced? tree)
  )
)

;functions for tree->stream
(define (stream-equal? s1 s2)
  (cond
    ((and (stream-empty? s1) (stream-empty? s2)) #t)
    ((or (stream-empty? s1) (stream-empty? s2)) #f)
    ((not (equal? (stream-first s1) (stream-first s2))) #f)
    (else (stream-equal? (stream-rest s1) (stream-rest s2)))
  )
)

(define (tree->stream tree order)
  (define (create-stream xs)
    (if (null? xs)
        empty-stream
        (stream-cons (car xs) (create-stream (cdr xs)))
    )
  )

  (cond
    ((not (tree-valid? tree)) empty-stream)
    ((empty-tree? tree) empty-stream)
    ((leaf? tree) (stream (root-tree tree)))
    ((equal? 'preorder order) (create-stream (preorder tree)))
    ((equal? 'inorder order) (create-stream (inorder tree)))
    ((equal? 'postorder order) (create-stream (postorder tree)))
    (else empty-stream)
  )
)

;functions for visualize tree
(define (get-width-num number)
  (define (loop number digits)
    (if (= number 0)
        digits
        (loop (quotient number 10) (+ digits 1))
    )
  )
  (if (= number 0)
      1
      (loop number 0)
  )
)

(define (func tree search-in max-width)
  (cond
    ((empty-tree? tree) max-width)
    ((> (get-width-num (root-tree tree)) max-width) (func (search-in tree) search-in (get-width-num (root-tree tree))))
    (else (func (search-in tree) search-in max-width))
  )
)
  
(define (get-left-node-max-width tree max-width)
  (if (not (tree-valid? tree))
      0
      (func tree left-tree max-width)
  )
)

(define (get-right-node-max-width tree max-width)
  (if (not (tree-valid? tree))
      0
      (func tree right-tree max-width)
  ) 
)

(define (tree-width tree)
  (define (loop tree init)
     (let*
      (
        (root (root-tree tree))
        (current-width (get-width-num root))
        (left (left-tree tree))
        (right (right-tree tree))
        (left-node-max-width (get-left-node-max-width tree current-width))
        (right-node-width (get-width-num (root-tree right)))
      )
      (cond
        ((empty-tree? tree) 0)
        ((leaf? tree) init)
        ((and (empty-tree? left) (leaf? right)) (+ 2 current-width (get-width-num (root-tree right)))) ;+ --
        ((empty-tree? left) (+ 2 current-width (loop right right-node-width))) ;+ --
        ((empty-tree? right) (loop left left-node-max-width))
        ((has-two-leaves? tree) (+ 2 left-node-max-width right-node-width)) ;+ --
        (else (+ 2 (loop left left-node-max-width) (loop right right-node-width))) ;+ --
      )
    )
  )
  (if (or (not (tree-valid? tree)) (empty-tree? tree))
     0
     (max (get-width-num (root-tree tree))
           (loop tree (get-width-num (root-tree tree)))
     )
  )
)

(define (tree-height tree)
  (define (loop tree)
    (let*
      (
        (left (left-tree tree))
        (right (right-tree tree))
      )
      (cond
        ((empty-tree? tree) 0)
        ((leaf? tree) 1)
        ((empty-tree? left) (loop right))
        ((empty-tree? right) (+ 2 (loop left)))
        ((and (leaf? left) (= (loop right) 1)) 3)
        ((has-two-leaves? tree) 3)
        (else (+ (loop right) (loop left)))
      )
    )
  )
  (if (not (tree-valid? tree))
      0
      (loop tree)
  )
)

(define (get-element f coordinates)
  (if (not (= (length coordinates) 2))
    -1
    (f coordinates)
  )
)

(define (get-x coordinates)
  (get-element car coordinates)
)

(define (get-y coordinates)
  (get-element cadr coordinates)
)

(define (coordinates-equal? c1 c2)
  (cond
    ((or (null? c1) (null? c2)) #f)
    ((and (= (get-x c1) (get-x c2)) (= (get-y c1) (get-y c2))) #t)
    (else #f)
  )
)

(define (contains-coordinates? coordinates xs)
  (cond
    ((or (null? coordinates) (null? xs)) #f)
    ((coordinates-equal? coordinates (car xs)) #t)
    (else (contains-coordinates? coordinates (cdr xs)))
  )
)

(define (add x i y xs)
  (define (loop x i y res)
    (cond
      ((>= i y) res)
      ((contains-coordinates? (list x i) res) (loop x (+ i 1) y res))
      (else (loop x (+ i 1) y (append res (list (list x i)))))
    )
  )
  (loop x i y xs)
)

(define (visualize tree)
  (define (print-dashes i len)
    (if (< i len)
        (begin
          (display #\-)
          (print-dashes (+ i 1) len)
        )
        0
    )
  )
  
  (define (build-cols i padding curr-row xs)
    (if (< i padding)
      (if (contains-coordinates? (list i curr-row) xs)
         (begin
           (display #\|)
           (build-cols (+ i 1) padding curr-row xs)
         )
         (begin
           (display #\space)
           (build-cols (+ i 1) padding curr-row xs)
         )
      )
      0
    )
  )

  (define all-tree-width (tree-width tree))
  (define (loop tree parent-spaces curr-row flag xs)
    (let*
      (
        (current-node (root-tree tree))
        (current-node-width (get-width-num current-node))
        (left (left-tree tree))
        (right (right-tree tree))
        (current-tree-width (tree-width tree))
        (left-tree-width (tree-width left))
        (number-dashes (- all-tree-width left-tree-width))
        (max-width-all-left (max left-tree-width current-node-width))
        (range (+ 2 (- max-width-all-left current-node-width)))
      )
      (cond
        ((empty-tree? tree) 1)
        ((leaf? tree) (begin
                        (if (char=? #\l flag)
                                (build-cols 0 parent-spaces curr-row xs)
                                (build-cols 0 0 curr-row xs)
                        )
                        (display current-node)
                        (display #\newline)
                       ))
        ((empty-tree? left) (begin
                                (if (char=? #\l flag)
                                  (build-cols 0 parent-spaces curr-row xs)
                                  (build-cols 0 0 curr-row xs)
                                )
                                (display current-node)
                                (display #\-)
                                (display #\-)
                                (loop right (+ 2 parent-spaces max-width-all-left) curr-row #\r xs)
                             ))
        ((empty-tree? right) (begin
                                (if (char=? #\l flag)
                                  (build-cols 0 parent-spaces curr-row xs)
                                  (build-cols 0 0 curr-row xs)
                                )
                                (display current-node)
                                (display #\newline)
                                (build-cols 0 parent-spaces curr-row xs)
                                (display #\|)
                                (display #\newline)
                                (loop left parent-spaces (+ curr-row 1) #\l xs)
                              ))
        ((and (not (empty-tree? left)) (= (tree-height right) 1)) (begin
                                                       (if (char=? #\l flag)
                                                           (build-cols 0 parent-spaces (+ curr-row 1) xs)
                                                           (build-cols 0 0 curr-row xs)
                                                       )
                                                       (display current-node)
                                                       (print-dashes 0 range)
                                                       (loop right parent-spaces curr-row #\r xs)
                                                       (build-cols 0 parent-spaces curr-row xs)
                                                       (display #\|)
                                                       (display #\newline)
                                                       (build-cols 0 parent-spaces (+ curr-row 1) xs)
                                                       (loop left parent-spaces (+ curr-row 1) #\r xs) ;give #\r because we have already printed parent-spaces and we don't want to print them again
                                                     ))
        (else (begin
                (display current-node)
                (print-dashes 0 range)
                (if (empty-tree? left)
                    (loop right (+ 2 parent-spaces max-width-all-left) curr-row #\r xs)
                    (loop right (+ 2 parent-spaces max-width-all-left) curr-row #\r (add parent-spaces curr-row (- (+ curr-row (tree-height right)) 1) xs))
                )
                (loop left parent-spaces curr-row #\l xs)
              )
         )
      )
    )
  )
  (cond
    ((or (not (tree-valid? tree)) (empty-tree? tree)) (display ""))
    (else (loop tree 0 1 #\r '()))
  )
)

;visualizing tree
#|
(visualize '(15 (2 () ()) (7 () ())))
(display #\newline)
(visualize '(1 () (3 (111 () ()) ())))
(display #\newline)
(visualize '(3 (111 () ()) ()))
(display #\newline)

(visualize '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))
(display #\newline)

(define complex '(10 (15 (8 () ()) (7 (1 () ()) (3 () ()))) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))))
(define sub '(10 (15 (2 () ()) (7 (1 () ()) (3 () ()))) ()))
(visualize complex)
(display #\newline)
(visualize sub)
(display #\newline)

(visualize '(1 (1500 (5 (4 () (3 () ())) ()) ()) (300 (15 () ()) (2 (100 () ()) (11 (121 () ()) (6 () (10 () ())))))))
(display #\newline)
(visualize '(1 (10000 (5 (6 () ()) (7 () ())) (8 (100 () (50000 () ())) (8000 (600000 () ()) ()))) (5000 () (11 (111010111 () (6154 () ())) (55041 (101100111 () ()) ())))))
(display #\newline)
(visualize '(16279419 () (354345 () (2343342 (34234 (34324123 () (32525253 () ())) (432434 () (325223 (12312352 () ()) ()))) (92348248 () ())))))
(display #\newline)
(visualize '(300 (9000 (4 (528239 () ()) (1000 (2 (3 (23523 () ()) ()) ()) (4234324 (1 (5 () (9090 () ())) ()) (89238 () ())))) ()) (938748923 (3478 () ()) (23 (111 (902390 (131 () ()) (3232 () ())) (4334 () ())) ()))))
|#