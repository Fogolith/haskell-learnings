{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage err = 
  case head $ words err of
              ['E'] -> LogMessage (Error . read . unwords . take 1 . tail $ words err) (getTimeStamp . unwords . drop 1 $ words err) (getMess err)
              ['I'] -> LogMessage Info (getTimeStamp err) (getMess err)
              ['W'] -> LogMessage Warning (getTimeStamp err) (getMess err)
              _ -> Unknown err

getTimeStamp :: String -> Int
getTimeStamp err = read . head . drop 1 $ words err

getMess :: String -> String
getMess err = last $ words err

parse :: String -> [LogMessage]
parse logs = parseLines $ lines logs
    where parseLines = map parseMessage

insert :: LogMessage -> MessageTree -> MessageTree
insert lm (Node mt1 (LogMessage et tmsmp errstring) mt2)  =
  case lm of
        (Unknown _) -> Node mt1 (LogMessage et tmsmp errstring) mt2
        (LogMessage _ timestamp _) -> 
           if timestamp > tmsmp 
              then case mt2 of
                (Node x y z) -> Node mt1 (LogMessage et tmsmp errstring) (insert lm (Node x y z))
                (Leaf) -> Node mt1 (LogMessage et tmsmp errstring) (Node Leaf lm Leaf)
              else case mt1 of
                (Leaf) -> Node (Node Leaf lm Leaf) (LogMessage et tmsmp errstring) mt2
                (Node x y z) -> Node (insert lm (Node x y z)) (LogMessage et tmsmp errstring) mt2
insert _ (Node _ (Unknown _) _) = Leaf
insert lm (Leaf) = Node Leaf lm Leaf

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right
inOrder (Node Leaf lm right) = lm : inOrder right
inOrder (Node left lm Leaf) = inOrder left ++ [lm]
inOrder (Node Leaf lm Leaf) = [lm]
inOrder Leaf = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong (y:ys) = let (x:xs) = inOrder $ build (y:ys)
                       in analyze x ++ whatWentWrong xs
whatWentWrong [] = [] 

analyze :: LogMessage -> [String]
analyze x = case x of 
  (LogMessage (Error x) _ str) -> [str | x >= 50]
  _ -> []
