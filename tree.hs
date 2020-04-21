module Tree where

data Tree a = Leaf | Node a (Tree a) (Tree a)  deriving Show

treeDepth :: Tree a -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

treeSum :: Tree a -> Int
treeSum Leaf = 0
treeSum (Node x leftSubtree rightSubtree) =
  x + (treeSum leftSubtree) + (treeSum rightSubtree)

isSortedTree :: Tree a -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted   = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x< maxVal && leftSorted && rightSorted

addNewMax :: Tree a -> Tree a
-- add a new max element to tree
addNewMax Leaf = Node 0 Leaf Leaf  -- input tree with no nodes
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)  -- this is the rightmost Node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree

treezip :: Tree a -> Tree a -> Tree a
treezip Leaf Leaf = Node (0,0) Leaf Leaf
treezip (Node x leftSubtree1 rightSubtree1) (Node y leftSubtree2 rightSubtree2) =
  Node (x,y) (treezip leftSubtree1 rightSubtree1) (treezip leftSubtree2 rightSubtree2)
