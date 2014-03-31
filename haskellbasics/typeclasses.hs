-- typeclasses define a set of functions that can have
-- different implementations depending on the type of
-- data they're given.

class BasicEq a where
  isEqual :: a -> a -> Bool
  isNotEqual :: a -> a -> Bool
  isNotEqual x y = not (isEqual x y)
  isEqual x y = not (isNotEqual x y)

-- instances of typeclasses define the needed typeclass
-- functions for a particular type

instance BasicEq Int where
  isEqual x y = x == y


data Color = Red | Green | Blue

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

-- can automatically derive Read, Show, Bounded, Enum,
-- Eq, and Ord

-- we can also 'subclass' typecallses:

class (Eq a) => BasicEq2 where
  isNotEqual2 :: a -> a -> Bool
-- this means to inherit the typeclass BasicEq2, it MUST 
-- also implement Eq first

-- newtype is used to rename an existing type. giving it
-- a distinct identity; the newtype is compiled / created
-- at COMPILE time, thus have no overhead. so its more space
-- and time efficient

newtype UniqueID = UniqueID Int
  deriving (Eq) 

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right




