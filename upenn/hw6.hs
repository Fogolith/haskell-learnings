-- http://www.seas.upenn.edu/~cis194/hw/06-laziness.pdf
fib :: Integer -> Integer
fib = error "todo"

fibs1 :: [Integer]
fibs1 = error "todo"

fibs2 :: [Integer]
fibs2 = error "todo"

{-
data Stream = Stream a

instance Show a => Show (Stream a) where
	show ...

streamToList :: Stream a -> [a]
streamToList = error "todo"

streamRepeat :: a -> Stream a
streamRepeat = error "todo"

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = error "todo"

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed = error "todo"

nats :: Stream Integer
nats = error "todo"

ruler :: Stream Interger
ruler = error "todo"

x :: Stream Integer
-}
