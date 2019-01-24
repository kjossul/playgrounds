module Listree where

data Listree a = Empty | Leaf a | Branch [Listree a] [Listree a] deriving (Eq, Show)

instance Functor Listree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Branch ls rs) = Branch (map (fmap f) ls) (map (fmap f) rs)

instance Foldable Listree where
  foldr f z Empty = z
  foldr f z (Leaf a) = f a z
  foldr f z (Branch ls rs) = foldr g (foldr g z rs) ls
    where g Empty z = z
          g (Leaf x) z = f x z
          g (Branch ls rs) z = foldr g (foldr g z rs) ls

conc :: Listree a -> Listree a -> Listree a
conc Empty t = t
conc t Empty = t
conc t1 t2 = Branch [t1] [t2]

myconcat = foldr conc Empty
myconcatMap f a = myconcat $ fmap f a

instance Applicative Listree where
  pure = Leaf
  fs <*> xs = myconcatMap (\f -> fmap f xs) fs
