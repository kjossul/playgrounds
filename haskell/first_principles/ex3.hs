module Ex3 where

main :: [Char] -> IO ()
main s = do
  putStrLn $ s ++ "!"
  putStrLn [s !! 4]
  putStrLn $ drop 9 s

currys = "Curry is awesome"
thirdLetter :: String -> Char
thirdLetter s = s !! 3

nthLetter :: Int -> Char
nthLetter n = currys !! n

rvrs :: String
rvrs = (drop 9 currys) ++ (take 4 $ drop 5 currys) ++ take 5 currys
