import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits x
      | x > 0 = map (toInteger . digitToInt) $ show x
      | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:xs) = snd $ foldr doubleIfEven (1, []) (x:xs)
  where doubleIfEven :: Integer -> (Integer, [Integer]) -> (Integer, [Integer])
        doubleIfEven y (count, list)
                   | even count = (count + 1, (y * 2) : list)
                   | otherwise = (count + 1, y : list)
doubleEveryOther [] = []

validate :: Integer -> Bool
validate number = sum (concatMap toDigits $ doubleEveryOther . toDigits $ number) `rem` 10 == 0

hanoi :: Integer -> a -> a -> a -> [(a, a)]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
