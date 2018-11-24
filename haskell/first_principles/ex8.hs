module Ex8 where

import Data.List (intersperse)

sumN :: (Eq a, Num a) => a -> a
sumN 0 = 0
sumN n = n + sumN (n-1)

mult :: Integral a => a -> a -> a
mult x 0 = 0
mult x y = x + mult x (y-1)

data DividedResult = Result Integer | DividedByZero

myDiv :: Integral a => a -> a -> (a, a)
myDiv x y = go x y 0
  where go n d q
          | n < d     = (q, n)
          | otherwise = go (n-d) d (q+1)

mc91 :: Int -> Int
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11

digitToWord :: Int -> String
digitToWord n = ["Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"] !! n

digits :: Int -> [Int]
digits n 
    | n < 10 = [n]
    | otherwise = let (d,m) = divMod n 10 
                  in digits d ++ [m]

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
