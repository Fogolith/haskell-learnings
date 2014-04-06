module Golf where

import Data.Map (insertWith)
import qualified Data.Map as M

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

-- should use unlines, etc here to shorten
-- histogram :: [Integer] -> String
histogram :: [Integer] -> String
histogram xs = tail $ foldl (\acc a -> getString a ++ acc) "\n==========\n0123456789\n" [1..maximum vals]
  where vals = map (\x -> fromLookup (M.lookup x kvs)) [0..9] 
        kvs = foldl (\ac y -> insertWith (+) y 1 ac) M.empty xs
        fromLookup (Just z) = z
        fromLookup Nothing = 0
        getString b = foldl (\accc c -> if (vals !! c) >= b then accc ++ "*" else accc ++ " " ) "\n" [0..9] 


