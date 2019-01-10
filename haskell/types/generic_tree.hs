module GenericTree where

data Gtree a = Leaf a | Branch [Gtree a]

instance Functor Gtree where
  fmap f (Leaf x)     = Leaf $ f x
  fmap f (Branch ts)  = Branch $ map (fmap f) ts

instance Applicative Gtree where
  pure = Leaf
  Leaf f <*> Leaf x = Leaf $ f x
  Branch [] <*> Branch [] = Branch []
  Branch (tf:tfs) <*> Branch (tx:txs) = Branch $ [tf <*> tx] ++ [Branch tfs <*> Branch txs]
