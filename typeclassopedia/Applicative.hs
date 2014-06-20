import Prelude hiding ((**))
import Control.Applicative

{-
	Using Applicative laws, prove that:
		pure f <*> x = pure (flip ($)) <*> x <*> pure f

	$ :: (a -> (b -> c)) -> a -> (b -> c)
	flip :: (a -> b -> c) -> (b -> a -> c)
	flip ($) :: (a -> (b -> c)) -> ((b -> c) -> a) ~ish

	(<*>) :: f (a -> b) -> f a -> f b

	   pure (flip ($)) <*> x
	is ~pure ($ x) <*> pure f => pure f <*> pure x == pure (f x) (law #2)

	ALSO:
	   ~pure ($ x) <*> pure f 
	is ~pure (f $ x) which == pure (f x) which == pure f <*> pure x == pure (f x)

instance Applicative Maybe where
	pure = Just
	(Just f) <*> (Just x) = Just (f x)
	_ <*> _ = Nothing

newtype ZipList a = ZipList { getZipList :: [a] }
 
instance Applicative ZipList where
  pure a = ZipList (repeat x)
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)
-}

class Functor f => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a,b)

instance Monoidal Maybe where
	unit = Just () 
	(Just f) ** (Just x) = Just (f, x)
	_ ** _ = Nothing

{-
	unit = pure ()
	pure a = fmap (const a) unit

	f (<*>) x = fmap (\(f,x) -> f x) f ** x
	f (**) x  = (,) <$> f <*> x
-}