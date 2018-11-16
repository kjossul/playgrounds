module Ex2 where

area r = pi * r ^ 2

-- (^) 10 $ 1 + 1 == 10 ^ (1 + 1)
-- 2 ^ 2 * 4 ^ 5 + 1 == (2 ^ (2)) * (4 ^ 5) + 1
waxOn = x * 5
  where 
    x = y ^ 2  -- each variable is indented on a separate line
    y = z + 8
    z = 7

triple x = 3 * x
waxOff x = triple x
