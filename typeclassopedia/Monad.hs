import Control.Applicative
import Control.Monad

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

{- Implement a Monad instance for the list constructor, []. Follow the types!

instance Monad [] where
	return a = [a]
	xs >>= f = concat (map f xs)

-- Implement a Monad instance for ((->) e).

instance Monad ((->) e) where
	return a = (\_ -> a)
	a >>= f = (\e -> f (a e)) -- i think
-}

-- ============ NEED TO REVISIT ============ --

data Free f a = Var a
              | Node (f (Free f a))

instance (Functor f) => Functor (Free f) where
	fmap f (Var a) = Var (f a)
	fmap f (Node a) = Node (fmap (fmap f) a)

instance (Functor f) => Applicative (Free f) where
	pure = Var
	(Var f) <*> a = fmap f a
	(Node a) <*> b = Node (fmap (<*> b) a)

instance (Functor f) => Monad (Free f) where
	return = Var
	(Var a) >>= f = f a
	a >>= f = join (fmap f a)

-- ============ NEED TO REVISIT ============ --

{- 
Implement (>>=) in terms of fmap (or liftM) and join.

	Had to do this for join, above

Now implement join and fmap (liftM) in terms of (>>=) and return.

	join :: (m (m a)) -> m a
	join a = a >>= id 

	fmap :: (a -> b) -> f a -> f b 
	fmap f a = a >>= (return . f)

return a >>= k  =  k a
m >>= return    =  m
m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
 
fmap f xs  =  xs >>= return . f  =  liftM f xs

return >=> g  =  g
g >=> return  =  g
(g >=> h) >=> k  =  g >=> (h >=> k)

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

Given the definition g >=> h = \x -> g x >>= h, prove the equivalence 
of the above laws and the usual monad laws.
-}