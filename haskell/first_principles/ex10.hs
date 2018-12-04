module Ex10 where

import Data.Time
import Data.List

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)), 
               DbNumber 9001, DbString "Hello, world!", 
               DbDate (UTCTime(fromGregorian 1921 5 1)(secondsToDiffTime 34123))]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map (\(DbDate x) -> x) . filter f
  where f (DbDate _) = True
        f _          = False

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map (\(DbNumber x) -> x) . filter f
  where f (DbNumber _) = True
        f _             = False

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = (realToFrac $ sum xs) / genericLength xs
  where xs = filterDbNumber db

stops   = "pbtdkg"
vowels  = "aeiou"

svs :: [Char] -> [Char] -> [(Char, Char, Char)]
svs s v = [(x, y, z) | x <- s, y <- v, z <- s, x == 'p']

nvn :: [String] -> [String] -> [(String, String, String)]
nvn n v = [(x, y, z) | x <- n, y <- v, z <- n]

seekritFunc :: String -> Int
-- Returns the ratio between total non whitespace characters and number of words
-- Can be considered as the average length of the words
seekritFunc x = div (sum (map length (words x))) (length (words x))

seekritFunc' :: String -> Float
seekritFunc' x = (sum . map genericLength . words $ x) / (genericLength $ words x)

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f bs = myOr $ map f bs

myElem :: Eq a => a -> [a] -> Bool
myElem x xs = foldr (\y b -> b || x == y) False xs 

myElem' :: Eq a => a -> [a] -> Bool
myElem' x xs = any (== x) xs 

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x y -> f x : y) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr (\x y -> if f x then x:y else y) [] xs

myFlatten :: [[a]] -> [a]
myFlatten = foldr (++) []

myFlatMap :: (a -> [b]) -> [a] -> [b]
myFlatMap f xs = myFlatten $ myMap f xs

myFlatten' :: [[a]] -> [a]
myFlatten' = myFlatMap id

myMaximumBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x y -> if f x y == GT then x else y) (last xs) xs

myMinimumBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x y -> if f x y == LT then x else y) (last xs) xs

