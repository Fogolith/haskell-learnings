module Golf where

skips :: [a] -> [[a]]
skips xs = [every y xs | y <- [1..length xs]]
    where every n (y:ys) 
                   | n <= length (y:ys) = (y:ys) !! (n - 1) : every n (drop n (y:ys))
                   | otherwise         = []
          every _ [] = []

localMaxima :: [Integer] -> [Integer]
localMaxima xs = snd $ foldl (\acc x -> checkMaxima x xs acc) (0, []) xs
  where checkMaxima y ys (n, cur)
                    | n == length ys - 1 = (n + 1, cur)
                    | n > 0             = (n + 1, if (y > ys!!(n-1)) && (y > ys!!(n+1)) then y:cur else cur)
                    | otherwise         = (n + 1, cur)


-- start from the bottom and prepend
histogram :: [Integer] -> String
histogram = "\n==========\n0123456789\n"
