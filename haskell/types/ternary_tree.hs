module TernaryTree where

data Color = Yellow | Blue deriving (Eq, Show)

data Ttree v = Leaf v Color | Node v Color (Ttree v) (Ttree v) (Ttree v) deriving (Eq, Show)

instance Functor Ttree where
  fmap f (Leaf v c)           = Leaf (f v) c
  fmap f (Node v c n1 n2 n3)  = Node (f v) c (fmap f n1) (fmap f n2) (fmap f n3)

instance Foldable Ttree where
  foldr f z (Leaf v c)          = f v z
  foldr f z (Node v c n1 n2 n3) =
    let v1 = foldr f v2 n1
        v2 = foldr f v3 n2
        v3 = foldr f z n3
    in f v v1

yellowSubTrees :: Ttree v -> [Ttree v]
yellowSubTrees (Leaf v Blue)           = []
yellowSubTrees (Leaf v Yellow)         = [Leaf v Yellow]
yellowSubTrees (Node v c n1 n2 n3)
  | c == Yellow && length sublist == 3 = [Node v Yellow n1 n2 n3]
  | otherwise                          = sublist
  where sublist = concat [yellowSubTrees n | n <- [n1, n2, n3]] 
