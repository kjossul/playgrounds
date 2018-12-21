data Blob a = Blob a (a -> a)

-- Can't derive Eq because of function in Type constructor
instance Eq a => Eq (Blob a) where
  (Blob a f) == (Blob b g) = (f a) == (g b)

instance Foldable Blob where
  foldr f z (Blob a g) = f (g a) z

instance Functor Blob where
  fmap f (Blob a g) = Blob (f $ g a) id

instance Applicative Blob where
  pure a = Blob a id
  (Blob f g) <*> (Blob a h) = Blob (g f $ h a) id 
