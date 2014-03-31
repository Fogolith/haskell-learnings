removeAt :: Int -> [a] -> (a, [a])
removeAt n [x]    = (x, [])
removeAt n (x:xs) = ((x:xs)!!(n-1), take (n-1) (x:xs) ++ drop n (x:xs))
