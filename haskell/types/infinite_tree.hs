data Itree a = Itree a (Itree a) (Itree a)

instance Show a => Show (Itree a) where
  show itree = go itree 2
    where go (Itree v _ _) 0 = "(" ++ show v ++ "(...))"
          go (Itree v l r) n = "(" ++ show v ++ "(" ++ go l (n-1) ++ ", " ++ go l (n-1) ++ "))" 

constItree :: a -> Itree a
constItree x = Itree x (constItree x) (constItree x)

list2Tree :: [a] -> Itree a
list2Tree (x:xs) = Itree x (list2Tree xs) (list2Tree xs)

instance Functor Itree where
  fmap f (Itree v l r) = Itree (f v) (fmap f l) (fmap f r)

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show, Eq)

takeLevels :: Itree a -> Int -> Tree a
takeLevels (Itree v _ _) 0 = Leaf v
takeLevels (Itree v l r) n = Node v (takeLevels l (n-1)) (takeLevels r (n-1))

applyAtLevel :: (a -> a) -> (Int -> Bool) -> Itree a -> Itree a
applyAtLevel f p itree = go f p itree 0
  where go f p (Itree v l r) n = let result
                                      | p n       = f v
                                      | otherwise = v
                                 in Itree result (go f p l (n+1)) (go f p r (n+1))


