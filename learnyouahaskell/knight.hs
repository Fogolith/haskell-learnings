import Control.Monad

type KnightPos = (Int, Int)

nextMoves :: KnightPos -> [KnightPos]
nextMoves (c,r) = do
  (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),
          (c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
  guard (c'`elem` [1..8] && r' `elem` [1..8])
  return (c',r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- nextMoves start
  second <- nextMoves first
  nextMoves second

canReachInThree :: KnightPos -> KnightPos -> Bool
canReachInThree start end = end `elem` in3 start


