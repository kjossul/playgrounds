module PeriodicStream where

data LolStream x = LolStream Int [x]

lol2lolstream :: [[a]] -> LolStream a
lol2lolstream xss = LolStream (length xss) (concat . repeat . concat $ xss)


instance Eq a => Eq (LolStream a) where
  LolStream _ [] == LolStream _ [] = True
  LolStream i (x:xs) == LolStream j (y:ys)
    | i < 0 || j < 0 = x == y && LolStream i xs == LolStream j ys
    | otherwise      = i == j && x == y && take i xs == take i ys

instance Foldable LolStream where
  foldr f z (LolStream i xs) = foldr f z xs

instance Functor LolStream where
  fmap f (LolStream i xs) 
    | i > 0     = LolStream i $ concat . repeat . fmap f . take i $ xs
    | otherwise = LolStream i $ fmap f xs

instance Applicative LolStream where
  pure x = LolStream 1 $ repeat x
  LolStream i fs <*> LolStream j xs = LolStream (i * j) $ fs <*> xs

lconc :: LolStream a -> LolStream a -> LolStream a
lconc (LolStream i xs) (LolStream j ys)
  | i < 0 || j < 0 = LolStream 0 $ xs ++ ys 
  | otherwise = LolStream (i+j) $ concat . repeat $ take i xs ++ take j ys

lconcat :: Foldable t => t (LolStream a) -> LolStream a
lconcat = foldr lconc (LolStream 0 [])

lconcatMap :: (a -> LolStream b) -> LolStream a -> LolStream b
lconcatMap f a = lconcat $ fmap f a

instance Monad LolStream where
  a >>= f = lconcatMap f a
