{-# LANGUAGE InstanceSigs #-}

module Ex20180905 where

apply f a = f a

data Dupl a = Dupl [a] [a] deriving (Eq, Show)

instance Foldable Dupl where
  foldr :: (a -> b -> b) -> b -> Dupl a -> b
  foldr f z (Dupl xs ys) = foldr f z (xs ++ ys) 

instance Functor Dupl where
  fmap :: (a -> b) -> Dupl a -> Dupl b
  fmap f (Dupl xs ys) = Dupl (map f xs) (map f ys)

instance Applicative Dupl where
  pure :: a -> Dupl a
  pure x = Dupl [x] [x]

  (<*>) :: Dupl (a -> b) -> Dupl a -> Dupl b
  (Dupl fs gs) <*> (Dupl xs ys) = Dupl (fs <*> xs) (gs <*> ys) 

dconc :: Dupl a -> Dupl a -> Dupl a
dconc (Dupl x1s y1s) (Dupl x2s y2s) = Dupl (x1s ++ x2s) (y1s ++ y2s)

dconcat :: Dupl (Dupl a) -> Dupl a
dconcat = foldr dconc (Dupl [] [])

dconcatMap :: (a -> Dupl b) -> Dupl a -> Dupl b
dconcatMap f d = dconcat $ fmap f d

instance Monad Dupl where
  (>>=) :: Dupl a -> (a -> Dupl b) -> Dupl b
  d >>= f = dconcatMap f d

