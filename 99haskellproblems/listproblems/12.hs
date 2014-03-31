decodeMod :: (Eq a) => [(a, Int)] -> [a]
decodeMod [] = []
decodeMod ((x, n):xs) = let expanded = replicate n x
                        in expanded ++ decodeMod xs

