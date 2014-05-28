-- http://www.seas.upenn.edu/~cis194/lectures/11-applicative2.html
module SExpr where

import AParser
import Control.Applicative
import Data.Char (isSpace, isUpper, isAlpha)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (++) <$> (pure <$> p) <*> (zeroOrMore p)

spaces :: Parser String
spaces = zeroOrMore (satisfy (isSpace))

ident :: Parser String
ident = oneOrMore (satisfy (isAlpha))

type Ident = String

data Atom = N Integer | I Ident
  deriving Show

data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

removeSpace :: Parser a -> Parser a
removeSpace a = spaces *> a <* spaces

parseAtom :: Parser Atom
parseAtom = removeSpace (N <$> posInt <|> I <$> ident)

parseSExpr' :: Parser [SExpr]
parseSExpr' = (removeSpace (char '(') *> zeroOrMore (parseSExpr) <* removeSpace (char ')'))

parseSExpr :: Parser SExpr
parseSExpr = (A <$> parseAtom) <|> (Comb <$> parseSExpr')