{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- http://www.seas.upenn.edu/~cis194/lectures/12-monads.html

module Risk where

import Control.Monad.Random
import Data.List
import Control.Monad

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

rollNtimes :: Int -> Rand StdGen [DieValue]
rollNtimes n = replicateM n die

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do
    aRolls <- rollNtimes (min 3 $ a)
    bRolls <- rollNtimes (min 2 $ d)
    let zlist = zip (sort aRolls) (sort bRolls)
    let atklen = length . filter (\(a, b) -> a > b) $ zlist
    return $ Battlefield (a - atklen) (d + atklen - (length zlist))

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

