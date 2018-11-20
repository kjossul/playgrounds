module Ex5 where
-- uncurrying exercises
curriedSum :: Int -> Int -> Int
curriedSum a b = a + b

uncurriedSum :: (Int, Int) -> Int
uncurriedSum (a, b) = a + b
-- type exercises
h :: (Num a, Num b) => a -> b -> b
h _ _ = 0  -- :t h 1.0 2 will always be (Num b) => b, the compiler remains broad

jackal :: (Ord a, Eq b) => a -> b -> a
jackal a b = a 
-- :t jackal "string" is Eq b => b -> [Char]
-- :t jackal 2 is 2 :: (Eq b, Num a, Ord a) => b -> a
-- The difference is that 2 will be passed as a Num, whle "string" as [Char]

kessel :: (Ord a, Num b) => a -> b -> a
kessel a b = a
-- :t kessel 1 is (Num a, Ord a) => b -> a
-- that is because I could define a Num that is not instance of Ord, breaking the contract

-- parametricity exercises
f1 :: a -> a
f1 a = a  -- only possible implementation

f2 :: a -> a -> a
f2 a b = a
--or f2 a b = b

f3 :: a -> b -> b
f3 a b = b
-- signature writing
functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

-- function writing
i :: a -> a
i = id

c :: a -> b -> a
c = curry fst

c'' :: b -> a -> b
c'' = c

c' :: a -> b -> b
c' = curry snd

r :: [a] -> [a]
r = id

co :: (b -> c) -> (a -> b) -> a -> c
co = (.)

a :: (a -> c) -> a -> a
a f x = x

a' :: (a -> b) -> a -> b
a' f x = f x
-- fix the sing module exercise
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x else sndString y
       where x = "Singin"
             y = "Somewhere"

f' :: Int -> String
f' = undefined

g :: String -> Char
g = undefined

h' :: Int -> Char
h' = g . f'

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g = fst . g . f
