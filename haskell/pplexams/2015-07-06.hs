module Ex20150706 where

ag1 :: Num a => a -> [a] -> [a]
ag1 = (:)

ag2 :: Num a => a -> a -> a
ag2 = (+)
