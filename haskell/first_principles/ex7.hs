module Ex7 where

mTh = \x -> \y -> \z -> x * y * z
addFive = (\x y -> (if x > y then y else x) + 5)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

nthDigit :: Integral a => a -> a -> a
nthDigit x = snd . (`divMod` 10) . fst . (`divMod` x)

tensDigit :: Integral a => a -> a
tensDigit = nthDigit 10

hunsD :: Integral a => a -> a
hunsD = nthDigit 100

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show
