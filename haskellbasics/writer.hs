import Control.Monad.Writer

writeLog :: Int -> Writer [String] Int
writeLog x = writer (x, ["Got number: " ++ show x])

addNums :: Writer [String] Int
addNums = do
  a <- writeLog 3
  b <- writeLog 5
  return (a*b)

explicitAdd :: Writer [String] Int
explicitAdd = writeLog 3 >>= (\x ->
              writeLog 5 >>= (\y ->
              return (x*y))) 

gcdWriter :: Int -> Int -> Writer [String] Int
gcdWriter a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ "mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcdWriter b (a `mod` b)
