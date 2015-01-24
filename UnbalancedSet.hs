{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UnbalancedSet where

import Control.Exception
import Data.Typeable (Typeable)

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

data ElementAlreadyExists = ElementAlreadyExists
                          deriving (Show, Typeable)

instance Exception ElementAlreadyExists where

-- Ex 2.3
insert' :: Ord a => a -> Tree a -> IO (Tree a)
insert' x Empty = return $ Node Empty x Empty
insert' x n@(Node l e r) = do
  res <- try (return $ go x n)
  return $ case res of
             Left (e :: ElementAlreadyExists) -> n
             Right t -> t
    where go x n | x < e = Node (insert x l) e r
                 | x > e = Node l e (insert x r)
                 | otherwise = throw ElementAlreadyExists

-- Ex 2.5a
complete :: Ord a => Int -> a -> Tree a
complete 0 a = Node Empty a Empty
complete s a = Node sub a sub
    where sub = complete (s-1) a

-- Ex 2.5b
sized :: Ord a => Int -> a -> Tree a
sized n a | n == 0 = Empty
          | n == 1 = Node Empty a Empty
          | even (n - 1) = let s = sized ((n - 1) `div` 2) a in Node s a s
          | otherwise = let (l, r) = create2 ((n - 1) `div` 2) in Node l a r
          where create2 n = (sized n a, sized (n+1) a)
