compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:y:[]) = if x == y
  then [x]
  else [x, y]
compress (x:y:xs) = if x == y 
  then compress (x : xs) 
  else x : compress (y : xs)
