fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a
  | f a == a  = a
  | otherwise = fixpoint f $ f a

data Set a = Set [a] deriving (Show)

instance Eq a => Eq (Set a) where
  (Set xs) == (Set ys) = (foldl (\x y -> x && elem y ys) True xs) &&
                         (foldl (\y x -> y && elem x xs) True ys)

setminus :: Eq a => Set a -> Set a -> Set a
setminus (Set xs) (Set ys) = Set $ filter (\x -> not $ elem x ys) xs

intersection :: Eq a => Set a -> Set a -> Set a
intersection (Set xs) (Set ys) = Set $ filter (\x -> elem x ys) xs

union :: Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (xs ++ ys)
