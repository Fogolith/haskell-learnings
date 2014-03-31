repli :: [a] -> Int -> [a]
repli [] n = []
repli [x] n = take n (repeat x)
repli ( x:xs ) n = take n (repeat x) ++ repli xs n
