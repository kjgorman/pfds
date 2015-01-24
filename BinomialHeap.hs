module BinomialHeap where

data Tree a = Node { rank :: Int, element :: a, children :: [Tree a] }
type Heap a = [Tree a]

empty :: Heap a
empty = []

isEmpty :: Heap a -> Bool
isEmpty = null

link :: Ord a => Tree a -> Tree a -> Tree a
link lt@(Node r x c) rt@(Node s y d) =
    if x < y
    then Node (r+1) x (rt : c)
    else Node (r+1) y (lt : d)

insTree :: Ord a => Tree a -> Heap a -> Heap a
insTree t [] = [t]
insTree t (s:ts) = if (rank t) < (rank s) then t:ts else insTree (link t s) ts

insert :: Ord a => a -> Heap a -> Heap a
insert x ts = insTree (Node 0 x []) ts

merge :: Ord a => Heap a -> Heap a -> Heap a
merge t [] = t
merge [] t = t
merge l@(t:ts) r@(s:ss) = if (rank t) < (rank s)
                          then t : (merge ts r)
                          else if (rank s) < (rank t)
                               then s : (merge l ss)
                               else insTree (link t s) (merge ts ss)

removeMinTree :: Ord a => Heap a -> (Tree a, Heap a)
removeMinTree [] = error "cannot remove from empty"
removeMinTree (t:[]) = (t, [])
removeMinTree (t:ts) = let (t', ts') = removeMinTree ts
                       in if (element t) < (element t') then (t, ts) else (t', t : ts')

findMin :: Ord a => Heap a -> a
findMin = element . fst . removeMinTree

deleteMin :: Ord a => Heap a -> Heap a
deleteMin ts = let ((Node _ x ts'), ss) = removeMinTree ts
               in merge (reverse ts') ss

-- Ex 3.5
findMin' :: Ord a => Heap a -> a
findMin' [] = error "cannot have a minimum in an empty list"
findMin' ts = go (tail ts) (element $ head ts)
    where
      go :: Ord a => Heap a -> a -> a
      go [] a = a
      go (h:hs) a = let hr = element h
                    in if (element h) < a then go hs (element h) else go hs a
