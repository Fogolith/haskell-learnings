-- rotate a list N places to the left
-- hint: user predefined functions length and (++)
-- e.g. rotate "abcdefgh" 3 == "defghabc"

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate [x] _ = [x]
rotate (x:xs) n 
  | n == 0     = (x:xs)
  | n > 0     = rotate (xs++[x]) (n-1)
  | otherwise = rotate (last xs:(x:init xs)) (n+1)
