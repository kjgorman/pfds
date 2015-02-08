module Deque where

data Deque a = Deque { front :: [a], rear :: [a] }
             deriving (Show)

rebalance :: Deque a -> Deque a
rebalance (Deque [] r) = uncurry (flip (\f r -> Deque (reverse f) r)) $ split r
rebalance (Deque f []) = uncurry (\f r -> Deque f $ reverse r) $ split f
rebalance d = d

split :: [a] -> ([a], [a])
split (x:[]) = ([x], [])
split xs = splitAt (length xs `div` 2) xs

empty :: Deque a
empty = Deque [] []

isEmpty :: Deque a -> Bool
isEmpty (Deque f r) = null f && null r

cons :: a -> Deque a -> Deque a
cons x (Deque f r) = rebalance $ Deque (x:f) r

snoc :: a -> Deque a -> Deque a
snoc x (Deque f r) = rebalance $ Deque f (x:r)

head :: Deque a -> Maybe a
head (Deque f r) = tryGet f r

last :: Deque a -> Maybe a
last (Deque f r) = tryGet r f

tryGet :: [a] -> [a] -> Maybe a
tryGet f r = case f of
               [] -> case r of { [] -> Nothing; s -> Just $ Prelude.last s }
               g -> Just $ Prelude.head g

tail :: Deque a -> Maybe (Deque a)
tail (Deque f r) = tryPop f r

init :: Deque a -> Maybe (Deque a)
init (Deque f r) = tryPop r f

tryPop :: [a] -> [a] -> Maybe (Deque a)
tryPop f r = case f of
               [] -> case r of { [] -> Nothing; s -> Just $ rebalance $ Deque [] (Prelude.init s) }
               g -> Just $ rebalance $ Deque (Prelude.tail g) r
