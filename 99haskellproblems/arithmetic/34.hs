coprime :: Int -> Int -> Bool
coprime x y = gcd x y == 1

totient :: Int -> Int
totient n = foldl (\acc x -> if coprime n x then acc + 1 else acc) 0 [1..(n-1)]
  
