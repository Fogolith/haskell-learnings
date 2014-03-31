split :: [a] -> Int -> ([a], [a])
split [] _ = ([],[])
split [x] _ = ([x],[])
split (x:xs) n
  | n > 0     = (x:rest, last)
  | otherwise = ([], x:xs)
    where (rest,last) = split xs (n - 1)
