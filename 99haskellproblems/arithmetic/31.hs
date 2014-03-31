isPrime :: Int -> Bool
isPrime n
  | n <= 1     = False
  | otherwise = foldl (\acc x -> if n `rem` x == 0 then acc && False else acc && True) True [2..(n-1)]
