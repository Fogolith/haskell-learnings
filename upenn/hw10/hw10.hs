-- http://www.seas.upenn.edu/~cis194/hw/10-applicative.pdf
module AParser where

import Control.Applicative
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


first :: (a -> b) -> (a,c) -> (b,c)
first f p = (f $ fst p, snd p)

instance Functor Parser where
    fmap f (Parser a) = Parser $ fmap (first f) . a

instance Applicative Parser where
    pure a  = Parser (\s -> Just (a,s))
    a <*> b = Parser f
        where
            f s = case (runParser a s) of -- run parser 1
                Just (x, y) -> case (runParser b y) of -- on success run parser 2
                    Nothing -> Nothing -- fail on Nothing (p 2)
                    Just (q, w) -> Just (x q, w) -- apply function returned from p1 on results of p2
                Nothing -> Nothing -- fail on Nothing (p 1)

-- LEFT-associative, so (,) <$> (char 'a') happens first
-- which gives us a parser that returns a partially applied (,) function
-- and the remaining string
abParser :: Parser (Char, Char)
abParser = (,) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (const . const ()) <$> (char 'a') <*> (char 'b')

intPair :: Parser [Integer]
intPair = (\x y z -> [x, z]) <$> posInt <*> (char ' ') <*> posInt

instance Alternative Parser where
    empty   = Parser (\_ -> Nothing)
    a <|> b = Parser f
        where
            f s = case (runParser a s) of Just x -> Just x
                                          Nothing -> (runParser b s)

intOrUppercase :: Parser ()
intOrUppercase = (const ()) <$> (satisfy isUpper) <|> (const ()) <$> (posInt)