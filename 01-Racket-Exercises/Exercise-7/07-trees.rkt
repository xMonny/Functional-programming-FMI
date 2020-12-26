#lang racket

(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define (empty-tree? tree) (null? tree))
(define (root-tree tree)
  (if (empty-tree? tree)
      '()
      (car tree)
  )
)
(define (left-tree tree)
  (if (empty-tree? tree)
      '()
      (cadr tree)
  )
)
(define (right-tree tree)
  (if (empty-tree? tree)
      '()
      (caddr tree)
  )
)

(define (tree-valid? tree)
  (cond
    ((empty-tree? tree) #t)
    ((not (list? tree)) #f)
    ((not (= (length tree) 3)) #f)
    ((not (number? (root-tree tree))) #f)
    (else (and (tree-valid? (left-tree tree)) (tree-valid? (right-tree tree))))
  )
)

(define (leaf? tree)
  (and (tree-valid? tree)
       (not (empty-tree? tree))
       (empty-tree? (left-tree tree))
       (empty-tree? (right-tree tree)))
)

(define example (make-tree 1
                           (make-tree 3 (make-leaf 8) empty-tree)
                           (make-tree 7 empty-tree (make-tree 9
                                                              (make-leaf 10)
                                                              (make-leaf 11)))))

(define example-bst (make-tree 8
                               (make-tree 3 (make-leaf 1) (make-tree 6 (make-leaf 4) (make-leaf 7)))
                               (make-tree 10 empty-tree (make-tree 14 (make-leaf 13) empty-tree))))

; искаме да проверим дали нещо се среща в дърво
; във всяка от задачите започваме с проверка за празно дърво
; елемент се среща в дърво, тогава когато:
; - или е корена на дървото
; - или се среща в лявото, или дясното поддърво (среща се = member-tree?)
(define (member-tree? x tree)
  (define (is-member? x tree)
    (cond
      ((empty-tree? tree) #f)
      ((= x (root-tree tree)) #t)
      (else (or (is-member? x (left-tree tree)) (is-member? x (right-tree tree))))
    )
  )
  (if (not (tree-valid? tree))
      #f
      (is-member? x tree)
  )

  ;Решение 2
  #|(cond
    ((not (tree-valid? tree)) #f)
    ((null? (filter (lambda (element) (= x element)) (flatten tree))) #f)
    (else #t)
  )|#
)

; искаме да намерим сумата на всички елементи в дървото
(define (sum-tree tree)
  (define (sum-all tree)
    (if (empty-tree? tree)
        0
        (+ (root-tree tree) (sum-all (left-tree tree)) (sum-all (right-tree tree)))
    )
  )

  (if (not (tree-valid? tree))
      0
      (sum-all tree)
  )
  
  ;Решение 2
  #|(if (not (tree-valid? tree))
      0
      (apply + (flatten tree))
  )|#
)

; искаме да намерим всички елементи на дадено ниво в дървото
; тук имахме две дъна:
; - отново гледаме за празно дърво
; - проверяваме дали сме поискали ниво 0 - тогава е окей да върнем корена на дървото ни (след като знаем, че не е празно)
; - иначе комбинираме (с append) резултатите за лявото и дясното поддърво, но с по-малко ниво
(define (tree-level n tree)
  (define (find-level n tree)
    (cond
      ((= n 0) (list (root-tree tree)))
      (else (append (find-level (- n 1) (left-tree tree))
                    (find-level (- n 1) (right-tree tree))))
      
    )
  )
  
  (if (or (empty-tree? tree) (not (tree-valid? tree)))
      empty-tree
      (find-level n tree)
  )
)

; искаме да приложим функцията f върху всички елементи на дървото (като истинската map, ама за дървета)
(define (tree-map f tree)
  (define (build tree)
    (cond
      ((empty-tree? tree) empty-tree)
      ((leaf? tree) (make-leaf (f (root-tree tree))))
      (else (make-tree (f (root-tree tree)) (build (left-tree tree)) (build (right-tree tree))))
    )
  )
  
  (if (not (tree-valid? tree))
      empty-tree
      (build tree)
  )
)

; искаме да върнем списък от елементите на дървото - ляво, корен, дясно
(define (tree->list tree)
  (define (to-list tree)
    (if (empty-tree? tree)
        '()
        (append (tree->list (left-tree tree)) (list (root-tree tree)) (tree->list (right-tree tree)))
    )
  )
  
  (if (not (tree-valid? tree))
      empty-tree
      (to-list tree)
  )
)

; искаме да проверим дали х се среща в двоичното наредено дърво tree
; тук правим итеративен процес (опашкова рекурсия)
(define (bst-member? x tree)
  (define (search x tree)
    (cond
      ((empty-tree? tree) #f)
      ((= x (root-tree tree)) #t)
      ((< x (root-tree tree)) (search x (left-tree tree)))
      (else (search x (right-tree tree)))
    )
  )
  
  (if (not (tree-valid? tree))
      #f
      (search x tree)
  )
)

; искаме да вкараме елемент в двоично наредено дърво (binary search tree - BST)
(define (bst-insert x tree)
  (define (insert x tree)
    (cond
      ((empty-tree? tree) (make-leaf x))
      ((< x (root-tree tree)) (make-tree (root-tree tree) (insert x (left-tree tree)) (right-tree tree)))
      (else (make-tree (root-tree tree) (left-tree tree) (insert x (right-tree tree))))
    )
  )

  (cond
    ((not (tree-valid? tree)) empty-tree)
    ((bst-member? x tree) tree)
    (else (insert x tree))
  )
)

; искаме да сортираме даден списък, използвайки tree->list и bst-insert
(define (sort xs)
  (tree->list (foldl bst-insert empty-tree xs))
)