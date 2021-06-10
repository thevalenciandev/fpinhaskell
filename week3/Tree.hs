data Tree = Leaf | Node Int Tree Tree

treeDepth :: Tree -> Int
treeDepth Leaf         = 0
treeDepth (Node _ r l) = 1 + max (treeDepth r) (treeDepth l)
