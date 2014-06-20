import Control.Applicative

{- Segue: The Trivial Monad -}
data W a = W a deriving Show

instance Functor W where
	fmap f (W a) = W (f a)

instance Applicative W where
	pure a = W a
	(W f) <*> (W a) = W (f a)

instance Monad W where
	return a = W a
	(W a) >>= f = f a

-- 1
g :: Int -> W Int -> W Int
g x y = y >>= return . (+ x)

-- 2
h :: W Int -> W Int -> W Int
-- h x y = (+) <$> x <*> y
h x y = x >>= (\z -> g z y)

-- 3
{-
a = 1
return a >>= f == f a ?
	return 1 => W 1
	W 1 >>= f => f 1
	f 1 == f a

m = W 1
m >>= return = m ?
	(W 1) >>= return => return 1
	return 1 => W 1
	W1 == W1

f = return . (+ 1)
g = return . (+ 2)
(m >>= f) >>= g == m >>= (\x -> f x >>= g) ?
	(W 1 >>= f) => f 1
	f 1 => W 2
	W 2 >>= g => g 2
	g 2 => W 4

	W 1 >>= (\x -> f x >>= g)
	f 1 => W 2
	W 2 >>= g => g2
	g 2 => W 4

	W 4 == W 4
-}

-- 4
join' :: W (W a) -> W a
join' a = a >>= id

-- Implement a Monad instance for the list constructor, []. Follow the types!
-- Implement a Monad instance for ((->) e).

data Free f a = Var a
              | Node (f (Free f a))

-- Implement Functor and Monad instances for Free f
-- You may assume that f has a Functor instance. This is 
-- known as the free monad built from the functor f.

instance Functor (Free f) where
	fmap = error "todo"

instance Applicative (Free f) where
	pure = error "todo"
	(<*>) = error "todo"

instance Monad (Free f) where
	return = error "todo"
	(>>=) = error "todo"