module Ex9 where

import Data.Char

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:xs) = Just x

myEft :: (Enum a, Ord a) => a -> a -> [a]
myEft x y
    | x > y     = []
    | x == y    = [x]
    | otherwise = x : myEft (succ x) y

mySplit :: Eq a => [a] -> a -> [[a]]
mySplit [] sep = []
mySplit (x:xs) sep
      | x == sep  = mySplit xs sep
      | otherwise = [l] ++ mySplit r sep
      where l = takeWhile (/= sep) (x:xs)
            r = dropWhile (/= sep) xs 

sqrCube :: Int -> [(Int, Int)]
sqrCube = (`take` [(x, y) | x <- squares, y <- cubes, x < 50, y < 50])
  where squares = [1^2..5^2]
        cubes   = [1^3..5^3]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _          = []
myZipWith f _ []          = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (,)

onlyUpper :: String -> String
onlyUpper = filter isUpper

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs

yell :: String -> String
yell = map toUpper

firstCapitalized :: String -> Char
firstCapitalized = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
  | x == True = True
  | otherwise = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr $ map f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (== x)

myReverse :: [a] -> [a]
myReverse []      = []
myReverse (x:xs)  = myReverse xs ++ [x]

myFlatten :: [[a]] -> [a]
myFlatten []       = []
myFlatten (xs:xss) = xs ++ myFlatten xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = myFlatten $ map f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:y:xs)
  | f x y > EQ  = myMaximumBy f (x:xs)
  | otherwise   = myMaximumBy f (y:xs) 

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f (x:y:xs)
  | f x y < EQ  = myMinimumBy f (x:xs)
  | otherwise   = myMinimumBy f (y:xs) 

myMax :: Ord a => [a] -> a
myMax = myMaximumBy compare

myMin :: Ord a => [a] -> a
myMin = myMinimumBy compare
