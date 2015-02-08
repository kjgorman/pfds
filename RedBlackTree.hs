module RedBlackTree where

data Colour = Red | Black
            deriving (Show)

data Tree a = Empty | Node Colour (Tree a) a (Tree a)
            deriving (Show)

colour :: Tree a -> Colour
colour (Node c _ _ _) = c

{- Exercise 3.8

  Invariant 1: No red node has a red child
  Invariant 2: Every path from the root to an empty node contains
               the same number of black nodes

  Show that in a tree of size n, the maximum depth of a node is
  2⌊log(n+1)⌋.

  We know that any given path must have the same number of black
  nodes. So the shortest path would be one that consists solely
  of black nodes, and the longest path must be one that has the
  same number of black nodes, interspersed with red nodes (we know
  from invariant one they must be exactly interspersed). Thus we know
  that given a shortest length path x then it must be the case that
  for the total size n:
  n >= 2ˣ - 1 (property of a perfect binary tree)
  n + 1 >= 2ˣ
  log (n+1) >= x
  And we know that given the shortest path has the same number of
  black nodes as the longest path, where the longest path has as
  many red nodes as black it follows that the longest path y is 2x
  and thus that y <= 2 log(n+1).
-}

member :: Ord a => a -> Tree a -> Bool
member _ Empty = False
member x (Node _ l v r) = if x < v
                          then member x l
                          else if x > v
                               then member x r
                               else True

insert ::Ord a => a -> Tree a -> Tree a
insert x s = let ins Empty = Node Red Empty x Empty
                 ins (Node c l v r) =
                     if x < v then lbalance $ Node c (ins l) v r
                     else if x > v then rbalance $ Node c l v (ins r)
                          else s
                 (Node _ l v r) = ins s
             in Node Black l v r

balance :: Tree a -> Tree a
balance (Node Black (Node Red (Node Red a x b) y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black (Node Red a x (Node Red b y c)) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black a x (Node Red (Node Red b y c) z d)) = Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black a x (Node Red b y (Node Red c z d))) = Node Red (Node Black a x b) y (Node Black c z d)
balance t = t

-- Exercise 3.9
fromOrdList :: Ord a => [a] -> Tree a
fromOrdList = foldr insert Empty

-- Exercise 3.10
lbalance, rbalance :: Tree a -> Tree a
lbalance (Node Black (Node Red (Node Red a x b) y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
lbalance (Node Black (Node Red a x (Node Red b y c)) z d) = Node Red (Node Black a x b) y (Node Black c z d)
lbalance t = t
rbalance (Node Black a x (Node Red (Node Red b y c) z d)) = Node Red (Node Black a x b) y (Node Black c z d)
rbalance (Node Black a x (Node Red b y (Node Red c z d))) = Node Red (Node Black a x b) y (Node Black c z d)
rbalance t = t
