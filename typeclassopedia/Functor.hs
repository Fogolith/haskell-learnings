data Pair a = Pair a a

data ITree a = Leaf (Int -> a) 
             | Node [ITree a]

{-

instance Functor (Either e) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x

instance Functor ((->) e) where
	fmap f a = (\x -> f (a x))

instance Functor ((,) e) where
	fmap f (e, x) = (e, f x)
-}

instance Functor Pair where
	fmap f (Pair a b) = Pair (f a) (f b)

instance Functor ITree where
	fmap f (Leaf g) = Leaf (\x -> f (g x))
	fmap f (Node xs) = Node (map (fmap f) xs)

{-
	* -> * which cannot be made an instance of Functor:
		data K a = K (a -> Int)

	The composition of two Functors is also a Functor:
		TRUE - see Data.Functor.Compose

	Give an example of a (bogus) Functor instance which satisfies the second 
	law but not the first:
	1. fmap id x = id x
	2. fmap (g . h) x = (fmap g . fmap h) x
-}

data Bunk a = A a | B a deriving (Show)

instance Functor Bunk where
	fmap f (A a) = B (f a)
	fmap f (B a) = B (f a)

{-
	x = A 1
	id x = A 1
	fmap id x = B 1

	fmap (g . h) x = B 3
	(fmap g . fmap h) x = B 3
-}

newtype List a = List [a] deriving (Show)

instance Functor List where
  fmap _ (List []) = List []
  fmap g (List (x:xs)) = List (g x : g x : fmap g xs)

{-
	Which laws are violated by the evil Functor instance for list shown above:
		both laws, or the first law alone? Give specific counterexamples.

	The first is violated
		x = [1]
		id x = [1]
		fmap id x = [1, 1]
		[1] =/= [1, 1]

	The second is violated
		x = [1]
		h = (+ 1)
		g = (+ 1)
		(g . h) = (+ 1 (+ 1))
		(fmap g . fmap h) x = (\a -> fmap g $ (\b -> fmap h b) a) = [3, 3, 3]
		fmap (g . h) x = [3, 3]
		[3, 3] =/= [3, 3, 3]
-}
