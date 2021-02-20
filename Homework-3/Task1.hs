module Task1 where
  data Tree a = EmptyTree | Node {
                              value :: a
                            , left  :: Tree a
                            , right :: Tree a
                        } deriving (Show, Read)

  data Strategy = Inorder | Postorder | Preorder deriving (Show, Read, Eq)

  leaf :: a -> Tree a
  leaf x = Node x EmptyTree EmptyTree

  values :: Strategy -> Tree a -> [a]
  values _ EmptyTree = []
  values s (Node root left right)
    | s == Preorder  = [root] ++ values s left ++ values s right
    | s == Inorder   = values s left ++ [root] ++ values s right
    | s == Postorder = values s left ++ values s right ++ [root]
