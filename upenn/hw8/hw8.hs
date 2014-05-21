{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List (sort)

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

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss xs = (maxWithBoss, maxWithoutBoss)
	where lists = unzip xs;
		  maxWithoutBoss = mconcat $ map (uncurry moreFun) xs;
  		  maxWithBoss = glCons boss $ maxWithoutBoss

maxFun' :: Tree Employee -> (GuestList, GuestList)
maxFun' (Node e []) = nextLevel e [(mempty, mempty)]
maxFun' (Node e l) = nextLevel e (map maxFun' l)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . maxFun'

formatList :: GuestList -> String
formatList (GL l f) = "Total fun: " ++ show f ++ "\n" ++ (unlines . sort . map empName $ l)

main :: IO ()
main = do
	file <- readFile "company.txt"
	putStr . formatList . maxFun . read $ file