{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Data.Char (toLower)
import Sized
import Buffer
import Editor

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

getScore :: Score -> Int
getScore (Score x) = x

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
scoreLine x = Single (scoreString x) x

instance Buffer (JoinList (Score, Size) String) where
	toString Empty = ""
	toString (Single m s) = s
	toString (Append m l r) = toString l ++ toString r
	fromString x = Single ((scoreString x), Size 1) x
	line n b = indexJ n b
 	replaceLine n _ b | indexJ n b == Nothing = b
	replaceLine n l b = takeJ (n-1) b +++ (fromString l) +++ dropJ n b
	numLines Empty = 0
	numLines (Single _ _) = 1
	numLines (Append (_, s) _ _) = getSize s
	value Empty = 0
	value (Single (v, _) _) = getScore v
	value (Append (v, _) _ _) = getScore v

main = runEditor editor $ Append (Score 26,Size 2) (Single (Score 8,Size 1) "Hello,") (Single (Score 18,Size 1) "I am Justin.")
