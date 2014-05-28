doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number 7!"
lucky x = "Nope!"

-- factorial is a function that takes an a and returns an a
-- so long as a in an Integral
factorial :: (Integral a) => a -> a
factorial 0 = 1  -- if factorial called with 0, return 1
factorial n = n * factorial (n - 1) -- otherwise factorial = n * factorial (n - 1)

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight"
  | bmi <= 25.0 = "You're normal"
  | bmi <= 30.0 = "You're fat"
  | otherwise = "You're really fat"

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b     = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b     = GT
  | a == b    = EQ
  | otherwise = LT

initials :: String -> String -> String
initials first last = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = first
        (l:_) = last

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- the function body here checks for two arrays.
-- if they're there, we apply the function to
-- x and y (first two elements) then recursively
-- call zipWith on the rest of the elements
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter (<=x) xs)
      biggerSorted = quicksort (filter (>=x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz a
  | odd a   = a:collatz((a*3) + 1)
  | even a  = a:collatz(a `div` 2)

numCollatz :: Int
numCollatz = length (filter (\xs -> length xs > 15) (map collatz [1..100]))
