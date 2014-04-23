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

{-

-- Deﬁne a data type of polymorphic streams, Stream
data Stream = Stream a

-- works by showing only some preﬁx of a stream (say, the ﬁrst 20 elements)
instance Show a => Show (Stream a) where
	show ...

-- Write a function to convert a Stream to an inﬁnite list
streamToList :: Stream a -> [a]
streamToList = error "todo"

-- generates a stream containing inﬁnitely many copies of the
-- given element
streamRepeat :: a -> Stream a
streamRepeat = error "todo"

-- applies a function to every element of a Stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = error "todo"

-- generates a Stream from a “seed” of type a, which is the
-- ﬁrst element of the stream, and an “unfolding rule” of type a -> a
-- which speciﬁes how to transform the seed into a new seed, to be
-- used for generating the rest of the stream.
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed = error "todo"

-- the inﬁnite list of natural numbers 0, 1, 2, . . .
nats :: Stream Integer
nats = error "todo"

-- corresponds to the ruler function 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
-- where the nth element in the stream (assuming the ﬁrst element
-- corresponds to n = 1) is the largest power of 2 which evenly
-- divides n. 
ruler :: Stream Interger
ruler = error "todo"

-}

{-

EXTRA CREDIT (TO DO)

-}
