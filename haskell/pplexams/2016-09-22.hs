module Ex20160922 where

filterHeads :: [[a]] -> [a]
filterHeads []      = []
filterHeads ([]:xs) = filterHeads xs
filterHeads (x:xs)  = head x : filterHeads xs

getTails :: [[a]] -> [[a]]
getTails xs = go xs []
  where go [] acc            = acc
        go ((h:[]):xs) acc   = go xs acc
        go ((h:rest):xs) acc = go xs (acc ++ [rest]) 
        go (_:xs) acc        = go xs acc

transpose :: [[a]] -> [[a]]
transpose xs = go xs []
  where go [] acc = acc
        go xs acc = go (getTails xs) (acc ++ [filterHeads xs])

 
