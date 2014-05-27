{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- http://www.seas.upenn.edu/~cis194/lectures/12-monads.html

module Risk where

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

{-
	The rules of attacking in Risk are as follows.
	• There is an attacking army (containing some number of units) and
	a defending army (containing some number of units).
	• The attacking player may attack with up to three units at a time.
	However, they must always leave at least one unit behind. That
	is, if they only have three total units in their army they may only
	attack with two, and so on.
	• The defending player may defend with up to two units (or only
	one if that is all they have).
	• To determine the outcome of a single battle, the attacking and
	defending players each roll one six-sided die for every unit they
	have attacking or defending. So the attacking player rolls one, two,
	or three dice, and the defending player rolls one or two dice.
	• The attacking player sorts their dice rolls in descending order. The
	defending player does the same.cis 194: homework 11 4
	• The dice are then matched up in pairs, starting with the highest
	roll of each player, then the second-highest.
	• For each pair, if the attacking player’s roll is higher, then one of
	the defending player’s units die. If there is a tie, or the defending
	player’s roll is higher, then one of the attacking player’s units die.

	Write battle, which simulates a single battle (as explained above) between two
	opposing armies.

	That is, it should simulate randomly rolling the
	appropriate number of dice, interpreting the results, and updating
	the two armies to reﬂect casualties. You may assume that each player
	will attack or defend with the maximum number of units they are
	allowed.

-}

battle :: Battlefield -> Rand StdGen Battlefield
battle = error "todo"

{-
	Now implement invade, which simulates an entire invasion attempt, that is, repeated calls
	to battle until there are no defenders remaining, or fewer than two
	attackers.
-}

invade :: Battlefield -> Rand StdGen Battlefield
invade = error "todo"

{-
	Finally, implement successProb, which runs invade 1000 times, and uses the results to compute a
	Double between 0 and 1 representing the estimated probability that
	the attacking army will completely destroy the defending army.
	For example, if the defending army is destroyed in 300 of the 1000
	simulations (but the attacking army is reduced to 1 unit in the other
	700), successProb should return 0.3
-}

successProb :: Battlefield -> Rand StdGen Double
successProb = error "todo"

