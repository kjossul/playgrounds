module Ex20150910 where

class Blup t where
  fisto :: t x y -> Maybe x
  fosto :: t x y -> Maybe y

data Blargh a b = Bip a b | Bop a | Bup

instance Blup Blargh where
  fisto (Bip a b) = Just a
  fisto (Bop a) = Just a
  fisto Bup = Nothing

  fosto (Bip a b) = Just b
  fosto _ = Nothing

data Blarf a b = La [a] | Lb [b]

instance Blup Blarf where
  fisto (La (a:as)) = Just a
  fisto _ = Nothing

  fosto (Lb (b:bs)) = Just b
  fosto _ = Nothing

smap :: (Int -> Int) -> (Int -> Int -> Int) -> [Int] -> Int -> [Int]
smap f op l t = go l 0 []
  where go [] _ _ = []
        go (x:xs) k res
          | k >= t = res
          | otherwise = let y = f x
                        in go xs (op k y) $ res ++ [y]

main = putStrLn . show $ smap (^2) (+) [1,2..] 100 
