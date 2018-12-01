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

