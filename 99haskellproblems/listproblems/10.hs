encode :: (Eq a) => [a] -> [(a, Int)]
encode [] = []
encode [x] = [(x, 1)]
encode (x:xs) = let (first, rest) = span (==x) xs
                in (x, length first + 1) : encode rest
