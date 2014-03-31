insertAt :: a -> [a] -> Int -> [a]
insertAt i [] _     = [i]
insertAt i [x] n    = if n > 1 then [x, i] else [i, x]
insertAt i (x:xs) n = take (n-1) (x:xs) ++ i : drop (n-1) (x:xs)
