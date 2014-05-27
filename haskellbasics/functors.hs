-- class Functor f where
--    fmap :: (a -> b) -> f a -> f b
--
-- that is, fmap takes a function from one type to another
-- and a functor applied with one type and returns a functor
-- applied with another type

-- so, types that act like a box can be functors: Maybe, Either,
-- lists, etc

data CMaybe a = CNothing | CJust a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust x) = CJust (f x)

-- applicative functors are functors that implement <*>
-- <*> allows us to map a function inside a functor over
-- another functor

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  Just f <*> something = fmap f something

-- basically, this allows us to do things like:
Just (+3) <*> Just 9
-- which gives
Just 12
-- and
pure (+3) <*> Just 10
Just 13
-- even:
pure (+) <*> Just 3 <*> Just 5
Just 8
-- which is the SAME as
(+) <$> Just 3 <*> Just 5
Just 8

-- liftA2 is equal to this ?
liftA2 (:) (Just 3) (Just 4)
Just [3,4]
(:) <$> (Just 3) <*> (Just 4)
Just [3,4]
