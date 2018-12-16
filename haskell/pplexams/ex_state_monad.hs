{-
 - exercise on monads
 - MPradella 2012
 -}

{- Text:
 - Define a simple variant of the state monad, where the state is an integer
 - number, and in which every application of a bind increments such number.
 -}

{-# LANGUAGE InstanceSigs #-}

-- Type definition
module Inc where

-- runInc produces (newInc, result)
newtype Inc a = Inc {runInc :: Int -> (a, Int)}

instance Functor Inc where
  fmap :: (a -> b) -> Inc a -> Inc b
  fmap f (Inc g) = Inc $ \s -> let (a, s') = g (s)
                               in  (f a, s')

instance Applicative Inc where
  pure :: a -> Inc a
  pure a = Inc $ \s -> (a, s + 1)

  (<*>) :: Inc (a -> b) -> Inc a -> Inc b
  (Inc ff) <*> (Inc g) = Inc $ \s -> let (f, s')  = ff s
                                         (a, s'') = g (s')
                                     in  (f a, s'')

instance Monad Inc where
  (>>=) :: Inc a -> (a -> Inc b) -> Inc b
  Inc f >>= g = Inc $ \s -> let (a, s')   = f s
                                Inc h     = g a
                            in  h s'


pass :: Inc Int
pass = return 0

-- 7 monadic operations -> state will be == 7
esmm :: Inc Int
esmm = do x <- return 1 
          pass
          pass
          x <- return (x+1)
          pass
          pass
          return (x+1)

test = let Inc a = esmm
       in  a 0
