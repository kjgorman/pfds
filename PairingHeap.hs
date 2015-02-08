module PairingHeap where

data Heap a = Empty | Heap { root :: a, heaps :: [Heap a] }
            deriving (Show)

findMin :: Heap a -> Maybe a
findMin Empty = Nothing
findMin h = Just $ root h

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h Empty = h
merge Empty h = h
merge l r = if root l <= root r
            then Heap (root l) (r : heaps l)
            else Heap (root r) (l : heaps r)

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (Heap x []) h

deleteMin :: Ord a => Heap a -> Heap a
deleteMin Empty = Empty
deleteMin h = mergePairs $ heaps h
    where mergePairs [] = Empty
          mergePairs (x:[]) = x
          mergePairs (x:y:zs) = merge (merge x y) $ mergePairs zs

{- Exercise 5.8 -}
data BinTree a = E | Node { left :: BinTree a, el :: a, right :: BinTree a }

instance Show a => Show (BinTree a) where
    show E = "E"
    show n = "Node ("++(show $ left n)++" <"++(show $ el n)++"> "++(show $ right n)++")"

toBinary :: Heap a -> BinTree a
toBinary Empty = E
toBinary h = let leanLeft (Heap r hs) b = Node (foldr leanLeft E hs) r b in leanLeft h E

bempty :: BinTree a
bempty = E

bisEmpty :: BinTree a -> Bool
bisEmpty E = True
bisEmpty _ = False

bfindMin :: BinTree a -> Maybe a
bfindMin E = Nothing
bfindMin b = Just $ el b

bmerge :: Ord a => BinTree a -> BinTree a -> BinTree a
bmerge E b = b
bmerge b E = b
bmerge l r = if (el l) <= (el r)
             then Node (Node (left r) (el r) (left l)) (el l) E
             else Node (Node (left l) (el l) (left r)) (el r) E

binsert :: Ord a => a -> BinTree a -> BinTree a
binsert x b = bmerge b $ Node E x E

bdeleteMin :: Ord a => BinTree a -> BinTree a
bdeleteMin h = mergePairs (left h)
    where mergePairs E = E
          mergePairs (Node l x (Node a y b)) = bmerge (bmerge (Node l x E) (Node a y E)) (mergePairs b)
          mergePairs h = h
