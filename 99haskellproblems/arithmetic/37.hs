import Data.List

isPrime :: Int -> Bool
isPrime n
  | n <= 1     = False
  | otherwise = foldl (\acc x -> if n `rem` x == 0 then acc && False else acc && True) True [2..(n-1)]

primeFactors :: Int -> [Int]
primeFactors n 
  | isPrime n = [n]
  | otherwise = getPrimeDivisor n : primeFactors (quot n (getPrimeDivisor n))
  where getPrimeDivisor n = head . filter (\x -> n `mod` x == 0) $ filter isPrime [2..n]

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = map (\xs -> (head xs, length xs)) . group . sort $ primeFactors n

totientImp :: [(Int, Int)] -> Int
totientImp ((a,b):xs) = product [(p - 1) * p ^ (c - 1) | (p,c) <- ((a,b):xs) ]
