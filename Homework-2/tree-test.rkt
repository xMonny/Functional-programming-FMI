#lang racket/base
(require rackunit rackunit/gui)
(require racket/string)
(require racket/stream)
(require "tree.rkt")

(define (expect-valid-expr str)
  (test-true
    (string-append "String '" str "' should be valid for tree")
    (tree? str)
  )
)

(define (expect-invalid-expr str)
  (test-false
    (string-append "String '" str "' should NOT be valid for tree")
    (tree? str)
  )
)

(test/gui 
 (test-suite
  "String validation with spaces expect to be true"
  (expect-valid-expr "  *  ")
  (expect-valid-expr "{5 * *}")
  (expect-valid-expr "{5 {2 * *} {3 * *}}")
  (expect-valid-expr " {500 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *} }}")
  (expect-valid-expr "{5 {22 {2 * *}      {6 * *}} {1 * {3 {111    * *} *}}}")
  (expect-valid-expr "{   3 {  1* {5 ** }}  {2**}   }   ")
  (expect-valid-expr "{11{4  {  3{0   *{1**}}*}    {5  {8*    *}{10{12*   *}{11*{3**} }}  }} {7{10**}{14  {100**}{5000*   {13915       {8*{1**}}*}}}    }}")
  (expect-valid-expr "{11 {4 * *} {5{1**}{10 *    *}}}")
  (expect-valid-expr " {3{1*{5**}} {2**}}")
  (expect-valid-expr " { 3  { 1  *   { 5  *     * }  } { 2 * * } } ")
  (expect-valid-expr "{2 {4 * *} *}")
  (expect-valid-expr "{2     {4     * *}               *} ")
 )

 (test-suite
  "String validation with NO spaces expect to be true"
  (expect-valid-expr "*")
  (expect-valid-expr "{5{4{3{0*{5{3**}*}}{1*{2{8*{10{1111*{1319{10000**}{11**}}}*}}{7*{9**}}}}}*} *}")
  (expect-valid-expr "{5*{6{7**}{8*{5{5*{4{4{3*{1**}}*}*}}*}}}}")
  (expect-valid-expr "{5{4{3{0*{5{3**}*}}{1*{2{8*{10{1111*{1319{10000**}{11**}}}*}}{7*{9**}}}}}*} {6{7**}{8*{5{5*{4{4{3*{1**}}*}*}}*}}}}")
  (expect-valid-expr "{2{4**}*}")
 )

 (test-suite
  "String validation with spaces expect to be false"
  (expect-invalid-expr "{5 5 *}")
  (expect-invalid-expr "{5 {5 *} *}")
  (expect-invalid-expr "{* 5}")
  (expect-invalid-expr "{5 * {* * *}}")
  (expect-invalid-expr "{5 {5 * *} {3 1 2}")
  (expect-invalid-expr " 2 {3{-1*{5**}}{2**}}")
  (expect-invalid-expr " {500 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *    *} }}")
  (expect-invalid-expr "{5 {{22 {2 * *}}      {6 * *}} {1 * {3 {111    * *} *}}}")
  (expect-invalid-expr "{   3 {  1* {5 ** }}{  {2**}   }   ")
  (expect-invalid-expr "{11{4  {  3{0   *{1**}}*}    {5  {8*  * *  *}}{10{12*   *}{11*{3**} }}  }} {7{10**}{14  {100**}{5000*   {13915       {8*{1**}}*}}}    }}")
  (expect-invalid-expr "{11 {4 * *}{} {5{1**}{10 *    *}}}")
 )

 (test-suite
  "String validation with NO spaces expect to be false"
  (expect-invalid-expr "{}")
  (expect-invalid-expr " {3{-1*{5**}}{2**}}")
  (expect-invalid-expr " {3{1*(5**)}{2**}}")
  (expect-invalid-expr " {3}{1*{5**}}{2**}}")
  (expect-invalid-expr " {{3{-1*{5**}}{2**}}")
 )
)

(define (expect-valid-tree tree)
  (test-true
    (string-append "Tree '" (tree->string tree) "' should be valid")
    (begin
      (display "Input tree: ")
      (display tree)
      (tree-valid? tree)
    )
  )
)

(define (expect-invalid-tree tree)
  (test-false
    (string-append "Tree should NOT be valid")
    (begin
      (display "Input tree: ")
      (display tree)
      (tree-valid? tree)
    )
  )
)

(test/gui 
 (test-suite
  "Tree validation expect to be true"
  (expect-valid-tree '())
  (expect-valid-tree '(1 () ()))
  (expect-valid-tree '(5 (2 () ()) ()))
  (expect-valid-tree '(5 () (3 () ())))
  (expect-valid-tree '(500 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) () ))))
  (expect-valid-tree '(3 (1 () (5 () ())) (2 () ())))
  (expect-valid-tree '(11 (4 (3 (0 () (1 () ())) ()) (5 (8 () ()) (10 (12 () ()) (11 () (3 () ()))))) (7 (10 () ()) (14 (100 () ()) (5000 () (13915 (8 () (1 () ())) ()))))))
  (expect-valid-tree '(11 (4 () ()) (5 (1 () ()) (10 () ()))))
  (expect-valid-tree '(3 (1 () (5 () ())) (2 () ())))
  (expect-valid-tree '(3 (1 () (5 () ())) (2 () ())))
  (expect-valid-tree '(5 (4 (3 (0 () (5 (3 () ()) ())) (1 () (2 (8 () (10 (1111 () (1319 (10000 () ()) (11 () ()))) ())) (7 () (9 () ()))))) ()) ()))
  (expect-valid-tree '(5 () (6 (7 () ()) (8 () (5 (5 () (4 (4 (3 () (1 () ())) ()) ())) ())))))
  (expect-valid-tree '(5 (4 (3 (0 () (5 (3 () ()) ())) (1 () (2 (8 () (10 (1111 () (1319 (10000 () ()) (11 () ()))) ())) (7 () (9 () ()))))) ()) (6 (7 () ()) (8 () (5 (5 () (4 (4 (3 () (1 () ())) ()) ())) ())))))
  (expect-valid-tree '(2 (4 () ()) ()))
 )

 (test-suite
  "Tree validation expect to be false"
  (expect-invalid-tree '(1))
  (expect-invalid-tree 5)
  (expect-invalid-tree '(1 (2 3) ()))
  (expect-invalid-tree '(100 ()))
  (expect-invalid-tree '(1 2 3))
  (expect-invalid-tree '(1 (1 2) ()))
  (expect-invalid-tree '(1 () (1 2)))
  (expect-invalid-tree '(() ()))
  (expect-invalid-tree '(()))
  (expect-invalid-tree '(5 (2 (2 3) ()) (5)))
  (expect-invalid-tree '(5 (1 ()) (3 () ())))
  (expect-invalid-tree '(500 (22 (2 () () (6 () ())) (1 () (3 (111 () ()) () )))))
  (expect-invalid-tree '(3 (1 () (5 () ())) (2)))
  (expect-invalid-tree '(11 (4 (3 (0 (1 () ()) (5 (8 () ()) (10 (12 () ()) (11 () (3 () ()))))) (7 (10 () ()) (14 (100 () ()) (5000 () (13915 (8 () (1 () ())) ()))))))))
  (expect-invalid-tree '(11 (4 (20 15 20 30) ()) (5 (1 () ()) (10 () ()))))
  (expect-invalid-tree '(3 (1 1 2 () (5 () ())) (2 () ())))
  (expect-invalid-tree '(3 (1 1 () (5 () ())) (2 () ())))
  (expect-invalid-tree '(5 (4 (3 (1 () (2 (8 () (10 (1111 () (1319 (10000 () ()) (11 () ()))) ())) (7 () (9 () ()))))) ()) ()))
  (expect-invalid-tree '(5 () (6 (7 () ()) (8 () 4 (5 (5 () (4 (4 (3 () (1 () ())) ()) ())) ())))))
  (expect-invalid-tree '(5 (4 (3 (0 () (5 (3 () () 5) ())) (1 () (2 (8 () (10 (1111 () (1319 (10000 () ()) (11 () ()))) ())) (7 () (9 () ()))))) ()) (6 (7 () ()) (8 () (5 (5 () (4 (4 (3 () (1 () ())) ()) ())) ())))))
  (expect-invalid-tree '(2 (4 () (1)) ()))
 )
)

(define (expect-correct-string-to-tree str tree)
  (test-true
    (string-append "String '" str "' should be converted correctly to tree")
    (begin
      (display "Converted tree: ")
      (display tree)
      (equal? (string->tree str) tree)
    )
  )
)

(test/gui 
 (test-suite
  "String converting to tree expect to be valid"
   (expect-correct-string-to-tree "*" '())
   (expect-correct-string-to-tree "{5 * *}" '(5 () ()))
   (expect-correct-string-to-tree "{5{4{3{0*{5{3**}*}}{1*{2{8*{10{1111*{1319{10000**}{11**}}}*}}{7*{9**}}}}}*} *}"
                                  '(5 (4 (3 (0 () (5 (3 () ()) ())) (1 () (2 (8 () (10 (1111 () (1319 (10000 () ()) (11 () ()))) ())) (7 () (9 () ()))))) ()) ()))
   (expect-correct-string-to-tree "{5{2540*{2**}}{3**}}" '(5 (2540 () (2 () ())) (3 () ())))
   (expect-correct-string-to-tree "{5{2540**}{3**}}" '(5 (2540 () ()) (3 () ())))
   (expect-correct-string-to-tree "{10987  {2540  {8987 *  *} *  }  {  3  *  {6894234 * *}  }  }" '(10987 (2540 (8987 () ()) ()) (3 () (6894234 () ()))))
   (expect-correct-string-to-tree "{10987  {2540  {8987 {  98723 * *  }  {101010**}} { 2189123   *   * }  }  {  3  {9832478 {89273928 * {98439 * *}} *}  {6894234 * *}  }  }"
                                  '(10987 (2540 (8987 (98723 () ()) (101010 () ())) (2189123 () ())) (3 (9832478 (89273928 () (98439 () ())) ()) (6894234 () ()))))
 )
)

(define (expect-correct-tree-to-string tree str)
  (test-true
    (string-append "String '" str "' should be correct conversion of input tree")
    (begin
      (display "Input tree: ")
      (display tree)
      (display "\nExpected string: ")
      (display (tree->string tree))
      (equal? (tree->string tree) str)
    )
  )
)

(define (expect-wrong-tree-to-string tree str)
  (test-false
    (string-append "String '" str "' should be incorrect conversion of input tree")
    (begin
      (display "Input tree: ")
      (display tree)
      (display "\nExpected string: ")
      (display (tree->string tree))
      (equal? (tree->string tree) str)
    )
  )
)

(test/gui 
 (test-suite
  "Tree converting to string expect to be valid"
   (expect-correct-tree-to-string '() "*")
   (expect-correct-tree-to-string '(1 () ()) "{1 * *}")
   (expect-correct-tree-to-string '(1 () (2 (3 () ()) ())) "{1 * {2 {3 * *} *}}")
   (expect-correct-tree-to-string '(1 (2 (3 () ()) ()) ()) "{1 {2 {3 * *} *} *}")
   (expect-correct-tree-to-string '(5 () ()) "{5 * *}")
   (expect-correct-tree-to-string '(5 (4 (3 (0 () (5 (3 () ()) ())) (1 () (2 (8 () (10 (1111 () (1319 (10000 () ()) (11 () ()))) ())) (7 () (9 () ()))))) ()) ())
                              "{5 {4 {3 {0 * {5 {3 * *} *}} {1 * {2 {8 * {10 {1111 * {1319 {10000 * *} {11 * *}}} *}} {7 * {9 * *}}}}} *} *}")
   (expect-correct-tree-to-string '(5 (2540 () (2 () ())) (3 () ())) "{5 {2540 * {2 * *}} {3 * *}}")
   (expect-correct-tree-to-string '(5 (2540 () ()) (3 () ())) "{5 {2540 * *} {3 * *}}")
   (expect-correct-tree-to-string '(10987 (2540 (8987 () ()) ()) (3 () (6894234 () ()))) "{10987 {2540 {8987 * *} *} {3 * {6894234 * *}}}")
   (expect-correct-tree-to-string '(10987 (2540 (8987 (98723 () ()) (101010 () ())) (2189123 () ())) (3 (9832478 (89273928 () (98439 () ())) ()) (6894234 () ())))
                                  "{10987 {2540 {8987 {98723 * *} {101010 * *}} {2189123 * *}} {3 {9832478 {89273928 * {98439 * *}} *} {6894234 * *}}}")
 )

 (test-suite
  "Tree converting to string expect to NOT be valid"
   (expect-wrong-tree-to-string '() " *")
   (expect-wrong-tree-to-string '(1 () ()) "{1* *}")
   (expect-wrong-tree-to-string '(1 () (2 (3 () ()) ())) "{1 *{2 {3 * *} *}}")
   (expect-wrong-tree-to-string '(1 (2 (3 () ()) ()) ()) "{1 {2 {3 * *} *} * }")
   (expect-wrong-tree-to-string '(5 () ()) "{ 5 * *}")
   (expect-wrong-tree-to-string '(5 (4 (3 (0 () (5 (3 () ()) ())) (1 () (2 (8 () (10 (1111 () (1319 (10000 () ()) (11 () ()))) ())) (7 () (9 () ()))))) ()) ())
                                "{5 {4{3 {0* {5 {3 **} *} } {1 * {2{8 * {10{1111  *  {1319 {10000 **} {11 * *}}} *}} {7 *{9 * *} } }  }    } *  } *}")
   (expect-wrong-tree-to-string '(5 (2540 () (2 () ())) (3 () ())) "{5{2540 *{2 **}} {3 **}} ")
   (expect-wrong-tree-to-string '(5 (2540 () ()) (3 () ())) "{5 {2540 **} {3 **}}")
   (expect-wrong-tree-to-string '(10987 (2540 (8987 () ()) ()) (3 () (6894234 () ()))) "{ 10987 { 2540 {8987 * *  } *} {3 *{ 6894234 * *}   }}")
   (expect-wrong-tree-to-string '(10987 (2540 (8987 (98723 () ()) (101010 () ())) (2189123 () ())) (3 (9832478 (89273928 () (98439 () ())) ()) (6894234 () ())))
                                "{10987{2540{8987{98723**}{101010**}}{2189123**}}{3{9832478{89273928*{98439**}}*}{6894234**}}}")
 )
)

(define (expect-balanced-tree tree)
  (test-true
    (string-append "Tree '" (tree->string tree) "' should be balanced")
    (balanced? tree)
  )
)

(define (expect-unbalanced-tree tree)
  (test-false
    (string-append "Tree '" (tree->string tree) "' should be unbalanced")
    (balanced? tree)
  )
)

(test/gui 
 (test-suite
  "Tree expect to be balanced"
   (expect-balanced-tree '())
   (expect-balanced-tree '(1 () ()))
   (expect-balanced-tree '(100 (5 () ()) (2 (1 () ()) ())))
   (expect-balanced-tree '(5 (3 (2 () ()) (4 () ())) (7 (6 () ()) ())))
   (expect-balanced-tree '(4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ()))))
   (expect-balanced-tree '(17 (11 (10 () ()) ()) (19 () ())))
   (expect-balanced-tree '(1 (2 (3 () ()) (4 () ())) (5 () (6 () ()))))
   (expect-balanced-tree '(18 (58 (72 (12 () ()) (11 () ())) (52 (55 () ()) (13 () ()))) (67 (45 (7 () ()) (51 () ())) (10 (56 () ()) (38 () ())))))
   (expect-balanced-tree '(6 (3 (1 () ()) (5 () ())) (9 (7 () ()) (11 () ()))))
 )

 (test-suite
  "Tree expect to be unbalanced"
   (expect-unbalanced-tree '(6 (2 (1 () ()) (5 (4 (3 () ()) ()) ())) ()))
   (expect-unbalanced-tree '(1 (2 (3 (4 () ()) ()) ()) (5 () (6 () ()))))
   (expect-unbalanced-tree '(1 (2 (5 (8 () ()) ()) (3 () ())) ()))
   (expect-unbalanced-tree '(50 (17 (9 () (14 (12 () ()) ())) (23 (19 () ()) ())) (76 (54 () (72 (67 () ()) ())) ())))
   (expect-unbalanced-tree '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
   (expect-unbalanced-tree '(7 (3 (1 () ()) (5 (4 () ()) (6 () ()))) (11 (9 (8 () (100 () ())) ()) (13 (12 () ()) (14 () ())))))
   (expect-unbalanced-tree '(40 (35 (30 (20 (10 (5 () ()) ()) ()) (34 () ())) (39 () ())) (70 () ())))
   (expect-unbalanced-tree '(10 (8 (2 (1 () ()) (6 (4 (3 () ()) (5 () ())) ())) ()) (11 () (14 (13 () ()) (16 () ())))))
   (expect-unbalanced-tree '(8 (5 (9 () ()) (7 (1 () ()) (12 (2 () ()) ()))) (4 () (11 (3 () ()) ()))))
 )
)

(define (expect-ordered-tree tree)
  (test-true
    (string-append "Tree '" (tree->string tree) "' should be ordered")
    (ordered? tree)
  )
)

(define (expect-unordered-tree tree)
  (test-false
    (string-append "Tree '" (tree->string tree) "' should be unordered")
    (ordered? tree)
  )
)

(test/gui 
 (test-suite
  "Tree expect to be ordered"
   (expect-ordered-tree '())
   (expect-ordered-tree '(1 () ()))
   (expect-ordered-tree '(100 () ()))
   (expect-ordered-tree '(5 (3 (2 (1 () ()) ()) (4 () ())) (7 (6 () ()) (9 () ()))))
   (expect-ordered-tree '(5 (7 (9 () ()) (6 () ())) (3 (4 () ()) (2 () (1 () ())))))
   (expect-ordered-tree '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
   (expect-ordered-tree '(50 (48 (30 (20 (15 () ()) (25 () ())) (32 (31 () ()) (35 () ()))) ()) (70 (65 () (67 (66 () ()) (69 () ()))) (90 () (98 (94 () ()) (99 () ()))))))
   (expect-ordered-tree '(50 (17 (12 (9 () ()) (14 () ())) (23 (19 () ()) ())) (72 (54 () (67 () ())) (76 () ()))))
   (expect-ordered-tree '(10 (8 (2 (1 () ()) (6 (4 (3 () ()) (5 () ())) ())) ()) (11 () (14 (13 () ()) (16 () ())))))
   (expect-ordered-tree '(30 (50 (60 (80 (100 () ()) (70 () ())) (55 (59 () ()) (54 () (53 () ())))) (45 (46 () ()) (41 () ()))) (20 (25 (29 () ()) ()) (10 (11 () ()) (9 () (5 (8 () ()) (2 () ())))))))
 )

 (test-suite
  "Tree expect to be unordered"
   (expect-unordered-tree '(3 (1 () ()) (2 () ())))
   (expect-unordered-tree '(5 (6 (2 (10 () ()) ()) (4 () ())) (7 (6 () ()) (9 () ()))))
   (expect-unordered-tree '(5 (7 (9 () ()) (6 () ())) (3 (4 () ()) (2 () (11 () ())))))
   (expect-unordered-tree '(0 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
   (expect-unordered-tree '(50 (48 (30 (20 (15 () ()) (24 () ())) (32 (51 () ()) (33 () ()))) ()) (72 (65 () (67 (66 () ()) (69 () ()))) (90 () (89 (94 () ()) (99 () ()))))))
   (expect-unordered-tree '(5 (1 (2 (9 () ()) (14 () ())) (23 (19 () ()) ())) (72 (54 () (67 () ())) (76 () ()))))
   (expect-unordered-tree '(100 (81 (20 (10 () ()) (16 (4 (3 () ()) (15 () ())) ())) ()) (18 () (14 (13 () ()) (16 () ())))))
   (expect-unordered-tree '(30 (5 (60 (80 (100 () ()) (70 () ())) (55 (59 () ()) (54 () (53 () ())))) (45 (46 () ()) (41 () ()))) (20 (25 (29 () ()) ()) (10 (11 () ()) (9 () (5 (8 () ()) (2 () ())))))))
 )
)

(define stream:empty                (stream '()))
(define stream:<1>                  (stream 1))
(define stream:<1.2.3>              (stream 1 2 3))
(define stream:<100.9.8.30.9.10.20> (stream 100 9 8 30 9 10 20))

(test/gui 
 (test-suite
  "Streams expect to be equal"
   (test-true "Empty streams should be equal" (stream-equal? stream:empty stream:empty))
   (test-true "Stream (1) should be equal to stream (1)" (stream-equal? stream:<1> stream:<1>))
   (test-true "Stream (1 2 3) should be equal to stream (1 2 3)" (stream-equal? stream:<1.2.3> stream:<1.2.3>))
   (test-true "Stream (100 9 8 30 9 10 20) should be equal to stream (100 9 8 30 9 10 20)" (stream-equal? stream:<100.9.8.30.9.10.20> stream:<100.9.8.30.9.10.20>))
 )
)

(define (expect-correct-preorder tree preordered-stream)
  (test-true
    (string-append "Tree '" (tree->string tree) "' should be correctly preordered")
    (begin
      (display "Input tree: ")
      (display tree)
      (display "\nPreordered tree (in stream): ")
      (display (stream->list preordered-stream))
      (stream-equal? (tree->stream tree 'preorder) preordered-stream)
    )
  )
)

(test/gui 
 (test-suite
  "Tree expect to be preordered in stream"
   (expect-correct-preorder '() empty-stream)
   (expect-correct-preorder '(5 () ()) (stream 5))
   (expect-correct-preorder '(5 (22 () ()) (1 () ())) (stream 5 22 1))
   (expect-correct-preorder '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))) (stream 5 22 2 6 1 3 111))
   (expect-correct-preorder '(10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))) (stream 10 15 2 7 5 22 2 6 1 3 111))
   (expect-correct-preorder '(5 (4 (3 (0 () (5 (3 () ()) ())) (1 () (2 (8 () (10 (1111 () (1319 (10000 () ()) (11 () ()))) ())) (7 () (9 () ()))))) ()) (6 (7 () ()) (8 () (5 (5 () (4 (4 (3 () (1 () ())) ()) ())) ()))))
                            (stream 5 4 3 0 5 3 1 2 8 10 1111 1319 10000 11 7 9 6 7 8 5 5 4 4 3 1))
 )
)

(define (expect-correct-inorder tree inordered-stream)
  (test-true
    (string-append "Tree '" (tree->string tree) "' should be correctly inordered")
    (begin
      (display "Input tree: ")
      (display tree)
      (display "\nInordered tree (in stream): ")
      (display (stream->list inordered-stream))
      (stream-equal? (tree->stream tree 'inorder) inordered-stream)
    )
  )
)

(test/gui 
 (test-suite
  "Tree expect to be inordered in stream"
   (expect-correct-inorder '() empty-stream)
   (expect-correct-inorder '(5 () ()) (stream 5))
   (expect-correct-inorder '(5 (22 () ()) (1 () ())) (stream 22 5 1))
   (expect-correct-inorder '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))) (stream 2 22 6 5 1 111 3))
   (expect-correct-inorder '(10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))) (stream 2 15 7 10 2 22 6 5 1 111 3))
   (expect-correct-inorder '(5 (4 (3 (0 () (5 (3 () ()) ())) (1 () (2 (8 () (10 (1111 () (1319 (10000 () ()) (11 () ()))) ())) (7 () (9 () ()))))) ()) (6 (7 () ()) (8 () (5 (5 () (4 (4 (3 () (1 () ())) ()) ())) ()))))
                            (stream 0 3 5 3 1 8 1111 10000 1319 11 10 2 7 9 4 5 7 6 8 5 3 1 4 4 5))
 )
)

(define (expect-correct-postorder tree postordered-stream)
  (test-true
    (string-append "Input tree '" (tree->string tree) "' should be correctly postordered")
    (begin
      (display "Input tree: ")
      (display tree)
      (display "\nPostordered tree (in stream): ")
      (display (stream->list postordered-stream))
      (stream-equal? (tree->stream tree 'postorder) postordered-stream)
    )
  )
)

(test/gui 
 (test-suite
  "Tree expect to be postordered in stream"
   (expect-correct-postorder '() empty-stream)
   (expect-correct-postorder '(5 () ()) (stream 5))
   (expect-correct-postorder '(5 (22 () ()) (1 () ())) (stream 22 1 5))
   (expect-correct-postorder '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))) (stream 2 6 22 111 3 1 5))
   (expect-correct-postorder '(10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))) (stream 2 7 15 2 6 22 111 3 1 5 10))
   (expect-correct-postorder '(5 (4 (3 (0 () (5 (3 () ()) ())) (1 () (2 (8 () (10 (1111 () (1319 (10000 () ()) (11 () ()))) ())) (7 () (9 () ()))))) ()) (6 (7 () ()) (8 () (5 (5 () (4 (4 (3 () (1 () ())) ()) ())) ()))))
                            (stream 3 5 0 10000 11 1319 1111 10 8 9 7 2 1 3 4 7 1 3 4 4 5 5 8 6 5))
 )
)

(define (expect-correct-tree-width tree width)
  (test-true
    (string-append "Tree '" (tree->string tree) "' for visualize should have width: " (number->string (tree-width tree)))
    (begin
      (display "Input tree: ")
      (display tree)
      (display "\nExpected width: ")
      (display width)
      (display "\nCorrect width: ")
      (display (tree-width tree))
      (equal? (tree-width tree) width)
    )
  )
)

(test/gui 
 (test-suite
  "Expect correct width of tree"
   (test-equal? "Empty tree should have width 0" (tree-width '()) 0)
   (test-equal? "Leaf 5 should have width 1" (tree-width '(5 () ())) 1)
   (test-equal? "Leaf 123 should have width 3" (tree-width '(123 () ())) 3)
   (expect-correct-tree-width '(12345 () ()) 5)
   (expect-correct-tree-width '(5 (123 () ()) ()) 3)
   (expect-correct-tree-width '(3 (111 () ()) ()) 3)
   (expect-correct-tree-width '(5 (22 () ()) (1 () ())) 5)
   (expect-correct-tree-width '(15 (2 () ()) (7 () ())) 5)
   (expect-correct-tree-width '(1 () (3 (111 () ()) ())) 6)
   (expect-correct-tree-width '(10 (150 () ()) (5 (22 (2 () ()) (6 () ())) ())) 10)
   (expect-correct-tree-width '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))) 13)
   (expect-correct-tree-width '(10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))) 20)
 )
)

(define (expect-correct-tree-height tree height)
  (test-true
    (string-append "Tree '" (tree->string tree) "' for visualize should have height: " (number->string (tree-height tree)))
    (begin
      (display "Input tree: ")
      (display tree)
      (display "\nExpected height: ")
      (display height)
      (display "\nCorrect height: ")
      (display (tree-height tree))
      (equal? (tree-height tree) height)
    )
  )
)

(test/gui 
 (test-suite
  "Expect correct height of tree"
   (test-equal? "Empty tree should have height 0" (tree-height '()) 0)
   (test-equal? "Leaf 5 should have height 1" (tree-height '(5 () ())) 1)
   (expect-correct-tree-height '(5 () (123 () ())) 1)
   (expect-correct-tree-height '(5 (22 () ()) (1 () ())) 3)
   (expect-correct-tree-height '(1 () (3 (111 () ()) ())) 3)
   (expect-correct-tree-height '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))) 6)
   (expect-correct-tree-height '(10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))) 9)
 )
)