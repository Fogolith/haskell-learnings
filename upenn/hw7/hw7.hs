{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Data.Char (toLower)
import Sized

data JoinList m a = Empty
				  | Single m a
				  | Append m (JoinList m a) (JoinList m a)
	deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = error "Empty JoinList"
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty o = o
(+++) o Empty = o
(+++) m1 m2 = Append (tag m1 <> tag m2) m1 m2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ i _ | i < 0 	  = Nothing
indexJ 0 (Single m a) = Just a
indexJ _ (Single m a) = Nothing
indexJ i (Append m a b)
	| i < 0        = Nothing
	| i < leftsize = indexJ i a
	| otherwise    = indexJ (i - leftsize) b
	where leftsize = getSize . size . tag $ a

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty 	     = Empty
dropJ i a | i <= 0   = a
dropJ _ (Single m a) = Empty
dropJ x (Append m a b)
	| x < leftsize = (dropJ x a) +++ b
	| otherwise = dropJ (x - leftsize) b
	where leftsize = getSize . size . tag $ a

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty 	       = Empty
takeJ i _ | i <= 0     = Empty
takeJ _ t@(Single m a) = t
takeJ x (Append m a b)
	| x <= leftsize = takeJ x a
	| otherwise     = a +++ (takeJ (x - leftsize) b)
	where leftsize = getSize . size . tag $ a


newtype Score = Score Int deriving (Eq, Ord, Show, Num)

instance Monoid Score where
    mempty = 0
    mappend = (+)

score :: Char -> Score
score c
  | toLower c `elem` "aeiourstln" = Score 1
  | toLower c `elem` "dg" = Score 2
  | toLower c `elem` "bcmp" = Score 3
  | toLower c `elem` "fhvwy" = Score 4
  | toLower c `elem` "k" =  Score 5
  | toLower c `elem` "jx" = Score 8
  | toLower c `elem` "qz" = Score 10
  | otherwise             = Score 0

scoreString :: String -> Score
scoreString [] = Score 0
scoreString [x] = score x
scoreString (x:xs) = score x `mappend` scoreString xs

scoreLine :: String -> JoinList Score String

{-
	To test that it's working, add the line import Scrabble to JoinList
	and write the following function to test out JoinLists annotated with scores:

	scoreLine :: String -> JoinList Score String

		Example:

	*JoinList> scoreLine "yay " +++ scoreLine "haskell!"

	Append (Score 23)
		(Single (Score 9) "yay ")
		(Single (Score 14) "haskell!")

	Exercise 4

	Finally, combine these two kinds of annotations. A pair
	of monoids is itself a monoid:

	instance (Monoid a, Monoid b) => Monoid (a,b) where
		mempty = (mempty, mempty)
		mappend (a1,b1) (a2,b2) = (mappend a1 a2, mappend b1 b2)

	This means that join-lists can track more than one type of annotation at once,
	in parallel, simply using a pair type

	Since we want to track size and score, you should provide a Buffer instance for
	the type:

		JoinList (Score, Size) String

	Note: enable FlexibleInstances and TypeSynonymInstances

	Finally, make a main function to run the editor interface using
	your join-list backend in place of the slow String backend (see
	StringBufEditor.hs for an example of how to do this). You should
	create an initial buffer of type JoinList (Score, Size) String and
	pass it as an argument to runEditor editor. Verify that the editor
	demonstration described in the section “Editors and Buffers” does
	not exhibit delays when showing the prompt.
-}
