module LeftistHeaps where

data Heap a = Empty
            | Heap { rank :: Int, element :: a, left :: Heap a, right :: Heap a }
              deriving (Show)

{- Ex 3.1
  Given a leftist heap of size n, show that the length of the right
  spine contains at most ⌊log(n+1)⌋ elements.

  Assume we have a fully balanced tree, with depth x, then it's size
  will be 2^x - 1. Every time that we add an element into the heap,
  the leftist property ensures that we either balance the heap, or
  we insert a new left child. Clearly for any unbalanced tree the
  total size will be less than the 2^x - 1 limit, and when we add an
  element to a balanced tree, we must create a new left child,
  incrementing x which also ensures n < 2^x - 1.

  So it must always be the case that:
  n <= 2^x - 1
  n + 1 <= 2^x
  log(n + 1) <= x
  and because x must be an integer we know we can floor the
  logarithm to give:
  ⌊log(n+1)⌋
  So we know that we must have at most this many elements in a
  fully balanced tree, which is the maximal situation for right
  spine length, as the leftist property ensures any addition will
  be a left child and increment x.
-}

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h Empty = h
merge Empty h = h
merge l r = if element l <= element r
            then makeT (element l) (left l) $ merge (right l) r
            else makeT (element r) (left r) $ merge l (right r)

rank' :: Heap a -> Int
rank' Empty = 0
rank' h = rank h

makeT :: a -> Heap a -> Heap a -> Heap a
makeT a l r = if rank' l >= rank' r
              then Heap (rank' r + 1) a l r
              else Heap (rank' l + 1) a r l

singleton :: a -> Heap a
singleton a = Heap 1 a Empty Empty

insert :: Ord a => a -> Heap a -> Heap a
insert a h = merge (singleton a) h

findMin :: Heap a -> a
findMin = element

deleteMin :: Ord a => Heap a -> Heap a
deleteMin h = merge (left h) (right h)

-- Ex 3.2
insert' :: Ord a => a -> Heap a -> Heap a
insert' a Empty = singleton a
insert' a h | a <= element h = makeT a (left h) (insert' (element h) (right h))
            | otherwise = makeT (element h) (left h) (insert' a (right h))

-- Ex 3.3
fromList :: Ord a => [a] -> Heap a
fromList xs = let hs = map singleton xs
              in head $ until ((1 ==) . length) mergeAdjacent hs

mergeAdjacent :: Ord a => [Heap a] -> [Heap a]
mergeAdjacent [] = []
mergeAdjacent (x:[]) = [x]
mergeAdjacent (x:y:zs) = (merge x y) : (mergeAdjacent zs)
