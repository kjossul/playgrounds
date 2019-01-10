module Ex20160722 where

iter :: (a -> a) -> a -> [a]
iter f v = v : iter f (f v)

data Rf a b = Rf [a] (a -> b)

instance (Show a, Show b) => Show (Rf a b) where
  show (Rf xs f) = (show xs) ++ " --> " ++ (show $ map f xs)

instance Functor (Rf a) where
  fmap f (Rf xs g) = Rf xs $ f . g

compose :: (Eq a, Eq b) => Rf a b -> Rf b c -> Rf a c
compose (Rf xs f) (Rf ys g) = Rf [x | x <- xs, elem (f x) ys] $ g . f
