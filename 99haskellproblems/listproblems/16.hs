import Data.List

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery (x:xs) n = dropIt (x:xs) n 1 where
  dropIt (x:xs) n i = (if i `mod` n == 0 then [] else [x]) ++ (dropIt xs n (i+1))
  dropIt [] _ _ = []
