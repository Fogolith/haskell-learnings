encodeMod :: (Eq a) => [a] -> [(a, Int)]
encodeMod [] = []
encodeMod [x] = [(x, 1)]
encodeMod (x:xs) = let (first, rest) = span (==x) xs
                in (x, length first + 1) : encodeMod rest
