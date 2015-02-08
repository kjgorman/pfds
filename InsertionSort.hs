module InsertionSort where

{- Exercise 4.2
   Although, I'm just going to use standard haskell lists
   because the evaluation scheme of haskell implies the
   formulation given by Okasaki. Which is kind of cheating
   but whatever -}

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x $ insertionSort xs

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x < y
                  then x : y : ys
                  else y : (insert x ys)

-- but will it fold?

insertionSort' :: Ord a => [a] -> [a]
insertionSort' = foldr insert []
