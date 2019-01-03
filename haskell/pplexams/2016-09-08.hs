module Ex20160908 where

checkFig :: [[Int]] -> Maybe Int
checkFig [] = Nothing
checkFig xs = let result = foldr (&&) True [checkRow (fst x) (snd x) | x <- zip xs [0,1..]]
              in if result
                then Just $ length xs
                else Nothing

-- Returns true if the list is all 0 except for a 1 at the n-th position
checkRow :: [Int] -> Int -> Bool
checkRow [] n = n < 0
checkRow (x:xs) n
  | x == 0 = n /= 0 && (checkRow xs $ n-1)
  | x == 1 = n == 0 && (checkRow xs $ n-1)
  | otherwise = False
