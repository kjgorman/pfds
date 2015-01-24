-- Ex 3.6
module RankedBinomialHeap where

data Tree a = Node { element :: a, children :: [Tree a] }
            deriving (Show)

data Heap a = Heap { rank :: Int, trees :: [Tree a] }
            deriving (Show)

empty :: Heap a
empty = Heap 0 []

isEmpty :: Heap a -> Bool
isEmpty = null . trees

ranks :: Heap a -> [Int]
ranks h = drop 1 . scanl (\r _ -> r - 1) (rank h) $ trees h

trank :: Tree a -> Int
trank t | null (children t) = 0
        | otherwise = 1 + (maximum $ map trank (children t))

link :: Ord a => Tree a -> Tree a -> Tree a
link l@(Node x ls) r@(Node y rs) = if x < y then Node x (r : ls) else Node y (l : rs)

insTree :: Ord a => Tree a -> Heap a -> Heap a
insTree l r = if isEmpty r
              then Heap (trank l) [l]
              else if lrank < (rank r)
                   then Heap lrank (l : ttrees)
                   else insTree (link l $ head $ trees r) (Heap (head $ ranks r) ttrees)
    where
      lrank = trank l
      ttrees = tail $ trees r

insert :: Ord a => a -> Heap a -> Heap a
insert x h = insTree (Node x []) h

pop :: Heap a -> (Tree a, Heap a)
pop h | isEmpty h = error "cannot pop from an empty heap"
      | otherwise = (head $ trees h, Heap (rank h - 1) (tail $ trees h))

merge :: Ord a => Heap a -> Heap a -> Heap a
merge l r | isEmpty l = r
          | isEmpty r = l
          | otherwise = if (rank l) < (rank r)
                        then let merged  = merge ts r
                             in Heap (rank merged + 1) (t : (trees merged))
                        else insTree (link t s) (merge ts ss)
                            where (t, ts) = pop l
                                  (s, ss) = pop r

removeMinTree :: Ord a => Heap a -> (Tree a, Heap a)
removeMinTree h | isEmpty h = error "cannot find the minimum of an empty heap"
                | otherwise = let (t:ts) = trees h
                              in if null ts then (t, empty)
                                 else let (t', ts') = removeMinTree (Heap 0 ts)
                                      in if (element t) < (element t')
                                         then (t, (Heap (maximum $ map trank ts) ts))
                                         else (t', Heap (maximum $ map trank ts) (t:ts))

findMin :: Ord a => Heap a -> a
findMin = element . fst . removeMinTree
