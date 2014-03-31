import Control.Monad.State
import System.Random

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((),a:xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  a <- pop
  pop

ackManip :: State Stack Int
ackManip = push 3 >>= (\_ ->
           pop)

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

explicitThree :: State StdGen (Bool, Bool)
explicitThree = randomSt >>= (\a ->
                randomSt >>= (\b ->
                return (a, b)))
