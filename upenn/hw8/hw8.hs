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


combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs boss lists = getMaxList $ foldr (\x acc -> (glCons boss x):acc) [] lists
		where getMaxList lists = foldr findMax (GL [] 0) lists;
			  findMax y@(GL l1 f) x@(GL l2 f1) = if f < f1 then x else y

-- takes the "boss" of the current sub-tree and a list of the results
-- for each subtree under that "boss" each result is a pair of GLs
-- the first GuestList in the pair is the best possible guest list
-- with the boss of that subtree; the second is the best possible guest
-- list without the boss of that subtree. nextLevel should then compute
-- the overall best guest list that includes the "boss" and the overall
-- best guest list that doesn't include the "boss"
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results = error "todo"

-- takes a company hierarchy as input and outputs a fun-maximizing
-- guest list. can use testCompany to test
maxFun :: Tree Employee -> GuestList
maxFun = error "todo"

-- implemnt main :: IO () so that it read the company.txt (use read to turn
-- it into a Tree Employee) and then prints out a formatted guest list,
-- sorted by first name, which looks like:
--	Total fun: 23923
--	Adam McMann
-- 	Adeline Anselme
-- ...
-- try to separate out "pure" computation from the IO as much as possible
-- i.e. don't let IO creep into helper functions