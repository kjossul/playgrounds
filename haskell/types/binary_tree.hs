module BinaryTree where

data Tree a = Nil | Leaf a | Branch (Tree a)(Tree a) deriving (Show, Eq)

instance Functor Tree where
  fmap f Nil            = Nil
  fmap f (Leaf v)       = Leaf $ f v
  fmap f (Branch t1 t2) = Branch (fmap f t1) (fmap f t2)

instance Foldable Tree where
  foldr f z Nil            = z
  foldr f z (Leaf v)       = f v z
  foldr f z (Branch t1 t2) = let v = foldr f z t2
                             in  foldr f v t1

tcompose :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
tcompose _ Nil _              = Nil
tcompose f (Leaf v) t         = fmap (f v) t
tcompose f (Branch t1 t2) t3  = Branch (tcompose f t1 t3) (tcompose f t2 t3)

tsearch :: Tree a -> [a]
tsearch t = foldr (\x y -> y ++ [x]) [] t

revtree :: Tree a -> Tree a
revtree t = fst $ go t stack
  where stack = tsearch t
        go Nil stack = (Nil, stack)
        go (Leaf _) (x:xs) = (Leaf x, xs)
        go (Branch t1 t2) xs = let r1 = go t1 xs
                                   r2 = go t2 $ snd r1 
                               in  (Branch (fst r1) (fst r2), snd r2)

