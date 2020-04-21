module Tree where

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

-- x = Node 5 (Node 1 Leaf Leaf) (Node 10 Leaf Leaf)
-- y = Node 10 (Node 6 Leaf Leaf) (Node 20 Leaf Leaf)
-- zip = Node (5,10) (Node (1,6) Leaf Leaf) (Node (10,20) Leaf Leaf)
treezip :: (Tree a) -> (Tree b) -> (Tree (a,b))
treezip Leaf Leaf = Leaf
treezip (Node _ _ _) Leaf = Leaf
treezip Leaf (Node _ _ _) = Leaf
treezip (Node x left1 right1) (Node y left2 right2) =
  Node (x,y) (treezip left1 left2) (treezip right1 right2)

treeunzip :: (Tree (a,a)) -> (Tree a, Tree a)
treeunzip Leaf  = (Leaf, Leaf)
treeunzip (Node (x,y) left1 right1) =
  let
    (l1, l2) = treeunzip left1
    (r1, r2) = treeunzip right1
  in
    ((Node x l1 l2), (Node y r1 r2))
