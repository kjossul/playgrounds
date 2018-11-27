module Cipher where

import Data.Char

shiftChar :: Int -> Char -> Char
shiftChar n c
  | isAlpha c = chr $ (+97) $ (`mod` 26) $ (+) n $ (ord c) - 97
  | otherwise = c
        
caesar :: Int -> String -> String
caesar n xs = map (shiftChar n . toLower) xs

