-- http://www.seas.upenn.edu/~cis194/hw/06-laziness.pdf
fib :: Integer -> Integer
fib n
	| n == 0    = 0
	| n == 1    = 1
	| otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Deﬁne a data type of polymorphic streams, Stream
-- Stream MUST be infinite, and have only a "cons" constructor
data Stream a = Cons a (Stream a) deriving (Eq)

-- works by showing only some preﬁx of a stream (say, the ﬁrst 20 elements)
instance Show a => Show (Stream a) where
	show = show . take 20 . streamToList 

-- Write a function to convert a Stream to an inﬁnite list
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- generates a stream containing inﬁnitely many copies of the
-- given element
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

-- applies a function to every element of a Stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- generates a Stream from a “seed” of type a, which is the
-- ﬁrst element of the stream, and an “unfolding rule” of type a -> a
-- which speciﬁes how to transform the seed into a new seed, to be
-- used for generating the rest of the stream.
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- the inﬁnite list of natural numbers 0, 1, 2, . . .
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

-- corresponds to the ruler function 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
-- where the nth element in the stream (assuming the ﬁrst element
-- corresponds to n = 1) is the largest power of 2 which evenly
-- divides n.
ruler :: Stream Integer
ruler = streamMap (nthRuler) nats

nthRuler :: Integer -> Integer
nthRuler x
	| value == [] = 0
	| otherwise   = head value
	where value = dropWhile (\z -> x `mod` (z * 2) /= 0) [y | y <- reverse [1..(x - 1)]]

{-

EXTRA CREDIT (TO DO)

-}
