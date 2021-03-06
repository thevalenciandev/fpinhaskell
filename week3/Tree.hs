data Tree = Leaf | Node Int Tree Tree deriving Show

treeDepth :: Tree -> Int
treeDepth Leaf         = 0
treeDepth (Node _ r l) = 1 + max (treeDepth r) (treeDepth l)

treeSum :: Tree -> Int
treeSum Leaf         = 0
treeSum (Node x r l) = x + treeSum r + treeSum l

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x l r) min max
    | x <= min || x > max = False
    | otherwise           = isSortedTree l min x && isSortedTree r x max

addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node x l Leaf) = Node x l (Node (x+1) Leaf Leaf)
addNewMax (Node x l r)    = Node x l (addNewMax r)

-- Returns a sorted list of values, if it's a binary search tree
treeToList :: Tree -> [Int]
treeToList Leaf = []
treeToList (Node x l r) = treeToList l ++ [x] ++ treeToList r

treeInsert :: Int -> Tree -> Tree
treeInsert x Leaf         = Node x Leaf Leaf
treeInsert x (Node y l r)
    | x < y     = Node y (treeInsert x l) r
    | otherwise = Node y l (treeInsert x r)
