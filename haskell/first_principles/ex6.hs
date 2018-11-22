module Ex6 where
import Data.List
-- Eq instances

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  (TisAn x) == (TisAn y) = x == y

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (Two x1 y1) == (Two x2 y2) = (x1 == x2) && (y1 == y2)

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  (TisAnInt x) == (TisAnInt y) = (x == y)
  (TisAString x) == (TisAString y) = (x == y)
  _ == _ = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  (Pair x1 y1) == (Pair x2 y2) = (x1 == x2) && (y1 == y2)

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (Tuple a1 b1) == (Tuple a2 b2) = (a1 == a2) && (b1 == b2)

data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
  (ThisOne x) == (ThisOne y) = x == y
  (ThatOne x) == (ThatOne y) = x == y
  _ == _ = False

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (Hello x) == (Hello y) = x == y
  (Goodbye x) == (Goodbye y) = x == y
  _ == _ = False

mySort :: [Char] -> [Char]
mySort = sort

signifier :: String -> Char
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n a = (f a) + (fromInteger n)


