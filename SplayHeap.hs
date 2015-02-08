module SplayHeap where

data Tree a = Empty | Node { left :: Tree a, elem :: a, right :: Tree a }

instance Show a => Show (Tree a) where
    show Empty = "E"
    show (Node l v r) = "Node ("++(show l) ++ " <"++(show v)++"> " ++ (show r)++")"

insert :: Ord a => a -> Tree a -> Tree a
insert x t = Node (smaller x t) x (bigger x t)

insert' :: Ord a => a -> Tree a -> Tree a
insert' x t = let (smaller, bigger) = partition x t
              in Node smaller x bigger

smaller :: Ord a => a -> Tree a -> Tree a
smaller _ Empty = Empty
smaller pivot (Node l v r) = if v > pivot then smaller pivot l
                             else case r of
                                    Empty -> Node l v Empty
                                    Node a y b ->
                                        if y > pivot then Node l v (smaller pivot a)
                                        else Node (Node l v a) y (smaller pivot b)

bigger :: Ord a => a -> Tree a -> Tree a
bigger _ Empty = Empty
bigger pivot (Node l v r) = if v <= pivot then bigger pivot r
                            else case l of
                                   Empty -> Node Empty v r
                                   Node a y b ->
                                       if y <= pivot then Node (bigger pivot b) v r
                                       else Node (bigger pivot a) y (Node b v r)

partition :: Ord a => a -> Tree a -> (Tree a, Tree a)
partition _ Empty = (Empty, Empty)
partition pivot n@(Node l v r) =
    if v <= pivot then
        case r of
          Empty -> (n, Empty)
          Node a y b ->
              if y <= pivot then
                  let (small, big) = partition pivot b
                  in (Node (Node l v a) y small, big)
              else
                  let (small, big) = partition pivot a
                  in (Node l v small, Node big y b)
    else
        case l of
          Empty -> (Empty, n)
          Node a y b ->
              if y <= pivot then
                  let (small, big) = partition pivot b
                  in (Node a y small, Node big v r)
              else
                  let (small, big) = partition pivot a
                  in (small, Node big y (Node b v r))

findMin :: Tree a -> Maybe a
findMin Empty = Nothing
findMin (Node Empty v _) = Just v
findMin (Node l v r) = findMin l

deleteMin :: Tree a -> Tree a
deleteMin Empty = Empty
deleteMin (Node Empty v r) = r
deleteMin (Node (Node Empty x b) y c) = Node b y c
deleteMin (Node (Node a x b) y c) = Node (deleteMin a) x (Node b y c)

{- Exercise 5.7 -}

splaySort :: Ord a => [a] -> [a]
splaySort xs = traverse $ foldr insert' Empty xs
    where traverse Empty = []
          traverse (Node l v r) = (traverse l) ++ [v] ++ (traverse r)
