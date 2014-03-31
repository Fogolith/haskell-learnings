import Data.List (maximumBy)
import Data.Function(on)

collatz :: Int -> [Int]
collatz x
  | x `mod` 2 == 0 = fromIntegral (x / 2) ++ collatz (fromIntegral (x / 2))
  | x == 1         = []
  | otherwise     = fromIntegral (3 * x + 1) ++ collatz (fromIntegral (3 * x + 1))


fourteen :: Int
fourteen = maximumBy (compare `on` length) $ map collatz [1000000..1]
