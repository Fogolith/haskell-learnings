import Data.List (tails)

combinations :: [a] -> Int -> [[a]]
combinations _ 0 = [[]]
combinations xs n = [ y:ys | y:xs' <- tails xs, ys <- combinations xs' (n-1)]
