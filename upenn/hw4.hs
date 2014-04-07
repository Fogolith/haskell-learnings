-- http://www.seas.upenn.edu/~cis194/hw/04-higher-order.pdf
fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter (even) 

fun2' :: Integer -> Integer
fun2' = sum . filter (even) . takeWhile (> 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1) 

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

-- generates balanced binary tree from list of values using
-- foldr (integer is height)
foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

insertTree :: a -> Tree a -> Tree a
insertTree x (Node h l val r) = Leaf
insertTree x Leaf = Leaf

-- returns True if and only if there are an odd number of True values
-- in the input list
xor :: [Bool] -> Bool
xor = odd . length . filter (==True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- implement foldl using foldr
myfoldl' :: (a -> b -> a) -> a -> [b] -> a
myfoldl' = error "todo"

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- Sieve of Sundaram algorithm (generate all prime numbers up to 2n+2)
sieves :: Integer -> [Integer]
sieves = error "todo"