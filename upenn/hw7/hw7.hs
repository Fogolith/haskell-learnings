module JoinList where

import Data.Monoid
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


data Score = Int
{-

	Mr. Dickens’s publishing company has changed their
	minds. Instead of paying him by the word, they have decided to pay
	him according to the scoring metric used by the immensely popular
	game of ScrabbleTM. You must therefore update your editor implementation 
	to count Scrabble scores rather than counting words.

	Hence, the second annotation you decide to implement is one
	to cache the ScrabbleTM score for every line in a buffer. Create a
	Scrabble module that deﬁnes a Score type, a Monoid instance for
	Score, and the following functions:

	score :: Char -> Score
	scoreString :: String -> Score

	The score function should implement the tile scoring values as
	shown at http://www.thepixiepit.co.uk/scrabble/rules.html; any
	characters not mentioned (punctuation, spaces, etc.) should be given
	zero points.

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