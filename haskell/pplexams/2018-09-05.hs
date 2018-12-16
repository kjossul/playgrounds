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

instance Monad Dupl where
  (>>=) :: Dupl a -> (a -> Dupl b) -> Dupl b
  (Dupl xs ys) >>= f = head $ map f xs
                           

