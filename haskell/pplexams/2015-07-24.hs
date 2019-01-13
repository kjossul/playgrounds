module Ex20150724 where

zipNullWith :: Num a => (a -> a -> a) -> [a] -> [a] -> [a]
zipNullWith op x y
  | length x > length y = zipWith op x (y ++ repeat 0)
  | otherwise           = zipWith op (x ++ repeat 0) y

instance Num a => Num [a] where
  x + y = zipNullWith (+) x y
  x - y = zipNullWith (-) x y
  x * y = zipNullWith (*) x y

  abs x = map abs x
  signum x = map signum x
  fromInteger x = [fromInteger x]

data TT = Leaf Int | List [TT] deriving (Show, Eq)

lile :: TT -> Bool
lile (Leaf n) = n == 0
lile (List tts) = ttelem (length tts) (List tts)
  where ttelem n (Leaf m) = n == m
        ttelem n (List []) = n == 0
        ttelem n (List (Leaf m : tts)) = n == m || ttelem n (List tts)
        ttelem n (List (List tta : ttb)) = ttelem n (List tta) || ttelem n (List ttb)

lilg :: TT -> Bool
lilg (Leaf n) = n == 0
lilg (List []) = True
lilg (List (Leaf _ : tts)) = lilg $ List tts
lilg (List (List tta : ttb)) = lile (List tta) && lilg (List tta) && lilg (List ttb)
