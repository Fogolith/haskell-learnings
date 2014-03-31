slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice [x] start end = if start == 0 then [x] else []
slice (x:xs) start end 
  | start > end = []
  | start == end = (x:xs)!!(start-1) : slice (x:xs) (start + 1) end
  | otherwise   = (x:xs)!!(start-1) : slice (x:xs) (start + 1) end
