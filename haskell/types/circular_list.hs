module CircularList where

data Clist a = Sentinel (Clist a) | Node a (Clist a)

instance Eq a => Eq (Clist a) where
  Sentinel (Sentinel _) == Sentinel (Sentinel _) = True
  Node x nx  == Node y ny                        = x == y && nx == ny
  _ == _ = False

instance Show a => Show (Clist a) where
  show (Sentinel _) = "Sentinel"
  show (Node x nx) = show x ++ " - " ++ show nx

list2clist :: [a] -> Clist a
list2clist [] = let new = Sentinel new
                in new
list2clist (x:xs) = Node x $ list2clist xs

clist2list :: Clist a -> [a]
clist2list (Sentinel _) = []
clist2list (Node x nx)  = x : clist2list nx

instance Functor Clist where
  fmap f (Node x nx) = let first = Node (f x) $ go nx first
                       in first
    where go (Sentinel _) first = Sentinel first
          go (Node y ny) first  = Node (f y) $ go ny first
