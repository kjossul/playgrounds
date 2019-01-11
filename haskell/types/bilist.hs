module BiList where

import Data.List
import Data.Ord

data Bilist a = Bilist [a] [a] deriving (Show, Eq)

bilist_ref :: Bilist a -> Int -> (a, a)
bilist_ref (Bilist xs ys) i = (xs !! i, ys !! i)

oddeven :: [a] -> Bilist a
oddeven xs = go xs (Bilist [] []) 0
  where go [] bl _ = bl
        go (x:xs) (Bilist ys zs) n
          | n == 0    = go xs (Bilist (ys ++ [x]) zs) 1
          | otherwise = go xs (Bilist ys (zs ++ [x])) 0

inv_oddeven :: Bilist a -> [a]
inv_oddeven bl = go bl [] 0
  where go (Bilist [] []) xs _ = xs
        go (Bilist xs ys) zs n
          | n == 0    = go (Bilist (tail xs) ys) (zs ++ [head xs]) 1
          | otherwise = go (Bilist xs (tail ys)) (zs ++ [head ys]) 0

sum_tuple :: Num a => (a, a) -> a
sum_tuple (x, y) = x + y

bilist_length :: Bilist a -> Int
bilist_length (Bilist xs ys)
  | length xs /= length ys = error "mismatching length"
  | otherwise              = length xs

bilist_max :: (Ord a, Num a) => Bilist a -> Int
bilist_max (Bilist [] []) = error "empty bilist"
bilist_max bl = snd . maximumBy (comparing fst) $ [(sum_tuple $ bilist_ref bl i, i) | i <- [0..bilist_length bl - 1]]
