module ListOfLists where

data Lt a = Lt Int [[a]] deriving (Eq, Show)

checkLt :: Lt a -> Bool
checkLt (Lt n xss) = foldr (\x y -> y && length x == n) True xss

sublist :: [a] -> [[a]]
sublist xs = let l = length xs
             in concat [go xs n | n <- [1..l]]
               where go _ 0      = [[]]
                     go [] _     = [[]]
                     go (x:xs) n = take n (x:xs) : (go xs n)
    
checklist :: Eq a => [a] -> Lt a -> Maybe [[a]]
checklist xs (Lt _ xss)
  | length result == 0 = Nothing
  | otherwise          = Just result
  where result = [ys | ys <- sublist xs, length ys > 0, not $ elem ys xss]

instance Functor Lt where
  fmap f (Lt n xss) = Lt n $ map (map f) xss
