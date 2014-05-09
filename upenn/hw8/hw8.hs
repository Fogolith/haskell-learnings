{-# OPTIONS_GHC -fno-warn-orphans #-}
-- http://www.seas.upenn.edu/~cis194/hw/08-IO.pdf
module Party where

import Employee
import Data.Monoid
import Data.Tree

-- adds an Employee to the GuestList and add their fun score without 
-- doing any kind of checks.
glCons :: Employee -> GuestList -> GuestList
glCons = error "todo"

-- GL [Employee] Fun
instance Monoid GuestList where
	mempty = GL [] 0
	mappend = error "todo"

-- takes two GuestLists and returns whichever one of them is more fun
moreFun :: GuestList -> GuestList -> GuestList
moreFun = error "todo"

-- implment:
-- treeFold :: ... -> Tree a -> b
-- for
{-
	data Tree a = Node {
		rootLabel :: a, -- label value
		subForest :: [Tree a] -- zero or more child trees
	}

use a fold to create a fn which takes an employee (the boss of some division) and 
the optimal guest list for each subdivision under him, and somehow combines
this information to compute the best guest list for the entire division

However, this obvious ﬁrst attempt fails! The problem is that we
don’t get enough information from the recursive calls. If the best
guest list for some subtree involves inviting that subtree’s boss, then
we are stuck, since we might want to consider inviting the boss of the
entire tree—in which case we don’t want to invite any of the subtree
bosses (since they wouldn’t have any fun anyway). But we might be
able to do better than just taking the best possible guest list for each
subtree and then excluding their bosses

The solution is to generalize the recursio to compure MORE information
in such a way tha twe can actually make the recursive step.
In particular, instead of computing the best guest list for a given
tree, we will compute TWO guest lists:
	1. the best possible guest list we can create if we invite the boss
		(that is, the employee at the root of the tree)
	2. the best possible guest list we can create if we DONT invite the
		boss
-}

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs = error "todo"

-- takes the "boss" of the current sub-tree and a list of the results
-- for each subtree under that "boss" each result is a pair of GLs
-- the first GuestList in the pair is the best possible guest list
-- with the boss of that subtree; the second is the best possible guest
-- list without the boss of that subtree. nextLevel should then compute
-- the overall best guest list that includes the "boss" and the overall
-- best guest list that doesn't include the "boss"
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel = error "todo"

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