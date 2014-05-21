{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Monoid
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp n f) (GL a gf) = GL (e:a) (f + gf)

instance Monoid GuestList where
	mempty = GL [] 0
	mappend (GL e _) gl = foldr (glCons) gl e

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = case (compare a b) of
                GT -> a
                _  -> b

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f e (Node r [])  = f r e
treeFold f e (Node r s) = f r (foldr (flip $ treeFold f) e s)

-- FIX, WRONG:
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = ((GL [] 0), (GL [] 0))
nextLevel boss [x] = x
nextLevel boss xs = foldr (\a acc -> maxTuple a acc) (GL [] 0, GL [] 0) xs 
	where maxTuple (a, b) (c, d) = (moreFun a c, moreFun b d);

maxFun' :: Tree Employee -> (GuestList, GuestList)
maxFun' (Node e l) = nextLevel e l

-- takes a company hierarchy as input and outputs a fun-maximizing
-- guest list. can use testCompany to test
maxFun :: Tree Employee -> GuestList
maxFun (Node e l) = uncurry moreFun $ nextLevel e (maxFun' l)

-- implemnt main :: IO () so that it read the company.txt (use read to turn
-- it into a Tree Employee) and then prints out a formatted guest list,
-- sorted by first name, which looks like:
--	Total fun: 23923
--	Adam McMann
-- 	Adeline Anselme
-- ...
-- try to separate out "pure" computation from the IO as much as possible
-- i.e. don't let IO creep into helper functions