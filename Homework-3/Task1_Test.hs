module Task1_Test where
    import Test.HUnit
    import Task1

    leaf_5 :: Tree Int
    leaf_5 = leaf 5

    emptyTree :: Tree Int
    emptyTree = EmptyTree

    treeWithTwoLeaves :: Tree Int
    treeWithTwoLeaves = Node 5 (leaf 22) (leaf 1)

    preorderedTreeWithTwoLeaves :: [Int]
    preorderedTreeWithTwoLeaves = [5, 22, 1]

    inorderedTreeWithTwoLeaves :: [Int]
    inorderedTreeWithTwoLeaves = [22, 5, 1]

    postorderedTreeWithTwoLeaves :: [Int]
    postorderedTreeWithTwoLeaves = [22, 1, 5]

    treeWithTwoLeavesString :: String
    treeWithTwoLeavesString = "(5 (22 () ()) (1 () ()))"

    complexTree :: Tree Int
    complexTree = 
        Node 10 (Node 15 (leaf 2) (leaf 7)) (Node 5 (Node 22 (leaf 2) (leaf 6)) (Node 1 EmptyTree (Node 3 (leaf 111) EmptyTree)))

    preorderedComplexTree :: [Int]
    preorderedComplexTree = [10, 15, 2, 7, 5, 22, 2, 6, 1, 3, 111]

    inorderedComplexTree :: [Int]
    inorderedComplexTree = [2, 15, 7, 10, 2, 22, 6, 5, 1, 111, 3]

    postorderedComplexTree :: [Int]
    postorderedComplexTree = [2, 7, 15, 2, 6, 22, 111, 3, 1, 5, 10]

    complexTreeString :: String
    complexTreeString = "(10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))"

    testPreorderEmptyTree :: Test
    testPreorderEmptyTree = "Empty tree preorder should be []" ~: [] ~=? values Preorder emptyTree

    testInorderEmptyTree :: Test
    testInorderEmptyTree = "Empty tree inorder should be []" ~: [] ~=? values Inorder emptyTree

    testPostorderEmptyTree :: Test
    testPostorderEmptyTree = "Empty tree postorder should be []" ~: [] ~=? values Postorder emptyTree

    testPreorderLeaf5 :: Test
    testPreorderLeaf5 = "preorder leaf 5 should be [5]" ~: [5] ~=? values Preorder leaf_5

    testInorderLeaf5 :: Test
    testInorderLeaf5 = "inorder leaf 5 should be [5]" ~: [5] ~=? values Inorder leaf_5

    testPostorderLeaf5 :: Test
    testPostorderLeaf5 = "postorder leaf 5 should be [5]" ~: [5] ~=? values Postorder leaf_5

    testPreorderTreeWithTwoLeaves :: Test
    testPreorderTreeWithTwoLeaves = ("Tree " ++ treeWithTwoLeavesString ++ " should be correct preordered") ~:
                                preorderedTreeWithTwoLeaves ~=? values Preorder treeWithTwoLeaves

    testInorderTreeWithTwoLeaves :: Test
    testInorderTreeWithTwoLeaves = ("Tree " ++ treeWithTwoLeavesString ++ " should be correct inordered") ~:
                            inorderedTreeWithTwoLeaves ~=? values Inorder treeWithTwoLeaves

    testPostorderTreeWithTwoLeaves :: Test
    testPostorderTreeWithTwoLeaves = ("Tree " ++ treeWithTwoLeavesString ++ " should be correct postordered") ~:
                                postorderedTreeWithTwoLeaves ~=? values Postorder treeWithTwoLeaves

    testPreorderComplexTree :: Test
    testPreorderComplexTree = ("Tree " ++ complexTreeString ++ " should be correct preordered") ~:
                                preorderedComplexTree ~=? values Preorder complexTree

    testInorderComplexTree :: Test
    testInorderComplexTree = ("Tree " ++ complexTreeString ++ " should be correct inordered") ~:
                                inorderedComplexTree ~=? values Inorder complexTree

    testPostorderComplexTree :: Test
    testPostorderComplexTree = ("Tree " ++ complexTreeString ++ " should be correct postordered") ~:
                                postorderedComplexTree ~=? values Postorder complexTree
    