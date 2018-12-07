module Ex23 where

-- rewriting of State monad
newtype Moi s a = Moi {runMoi :: s -> (s, a)}

instance Functor (Moi s) where
  --fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (s', a) = g s 
                               in (s', f a)

instance Applicative (Moi s) where
  -- pure :: a -> Moi s a
  pure a = Moi $ \s -> (s, a)
  -- (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> let (sf, h) = f s
                                        (sg, a) = g sf
                                    in (sg, h a)

instance Monad (Moi s) where
  -- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s -> let (sf, a) = f s
                                  Moi h   = g a
                                  (sg, b) = h sf
                              in (sg, b)

-- Utility functions

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put x = Moi $ \s -> (x, ())

evalState :: Moi s a -> s -> a
evalState act = snd . runMoi act

-- Usage example

playGame :: String -> Moi (Bool, Int) Int
playGame []     = do
  (_, score) <- get
  return score

playGame (x:xs) = do
  (on, score) <- get
  case x of
    'a' | on -> put (on, score + 1)
    'b' | on -> put (on, score - 1)
    'c'      -> put (not on, score)
    _        -> put (on, score)
  playGame xs

startState = (False, 0)
main = print $ evalState (playGame "abcaaacbbcabbab") startState
