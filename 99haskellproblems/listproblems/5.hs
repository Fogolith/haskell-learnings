myReverse :: [a] -> [a]
myReverse (x:[]) = [x]
myReverse (x:y:[]) = [y, x]
myReverse (x:xs) = myReverse xs ++ [x]
