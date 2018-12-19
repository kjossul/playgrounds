module Ex20180720 where

data DeepList a = Leaf a | DeepList [DeepList a] deriving Eq

instance Show a => Show (DeepList a) where
  show (Leaf a)       = show a
  show (DeepList dls) = show dls

instance Functor DeepList where
  -- fmap :: (a -> b) -> DeepList a -> DeepList b
  fmap f (Leaf a)       = Leaf $ f a
  fmap f (DeepList dls) = DeepList $ map (fmap f) dls

fep :: [a] -> DeepList a
fep xs = fep' xs xs

fep' :: [a] -> [a] -> DeepList a
fep' [] xs    = DeepList [Leaf x | x <- xs]
fep' (x:xs) l = DeepList [Leaf x, fep' xs l, Leaf x]
