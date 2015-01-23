module UnbalancedSet where

data Tree a = Empty | Node (Tree a) a (Tree a)
            deriving (Show)

empty :: Tree a
empty = Empty

member :: Ord a => a -> Tree a -> Bool
member _ Empty = False
member x (Node l e r) | x < e = member x l
                      | x > e = member x r
                      | otherwise = True

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x n@(Node l e r) | x < e = Node (insert x l) e r
                        | x > e = Node l e (insert x r)
                        | otherwise = n


-- Ex 2.2
member' :: Ord a => a -> Tree a -> Bool
member' _ Empty = False
member' x n@(Node _ e _) = go x n e
    where
      go :: Ord a => a -> Tree a -> a -> Bool
      go x Empty a = x == a
      go x (Node l e r) a | x < e = go x l a
                          | otherwise = go x r e
