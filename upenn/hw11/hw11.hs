-- http://www.seas.upenn.edu/~cis194/lectures/11-applicative2.html
module SExpr where

import AParser
import Control.Applicative

{-
	There are two functions
	you should implement: zeroOrMore takes a parser as input and runs
	it consecutively as many times as possible (which could be none, if
	it fails right away), returning a list of the results. zeroOrMore always
	succeeds. oneOrMore is similar, except that it requires the input parser
	to succeed at least once. If the input parser fails right away then
	oneOrMore also fails.
	For example, below we use zeroOrMore and oneOrMore to parse a
	sequence of uppercase characters. The longest possible sequence of
	uppercase characters is returned as a list. In this case, zeroOrMore
	and oneOrMore behave identically:

	*AParser> runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
	Just ("ABC","dEfgH")
	*AParser> runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
	Just ("ABC","dEfgH")

	*AParser> runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"
	Just ("","abcdeFGh")
	*AParser> runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"
	Nothing
-}

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = error "todo"

oneOrMore :: Parser a -> Parser [a]
oneOrMore = error "todo"

{-
	spaces should parse a consecutive
	list of zero or more whitespace characters (use the isSpace function
	from the standard Data.Char module)
-}

spaces :: Parser String
spaces = error "todo"

{-
	ident should parse an identiﬁer, which for our purposes
	will be an alphabetic character (use isAlpha) followed by zero or
	more alphanumeric characters (use isAlphaNum). In other words, an
	identiﬁer can be any nonempty sequence of letters and digits, except
	that it may not start with a digit:

	*AParser> runParser ident "foobar baz"
	Just ("foobar"," baz")
	*AParser> runParser ident "foo33fA"
	Just ("foo33fA","")
	*AParser> runParser ident "2bad"
	Nothing
	*AParser> runParser ident ""
	Nothing
-}

ident :: Parser String
ident = error "todo"

{-
	S-expressions are a simple syntactic format for tree-structured data
	originally developed as a syntax for Lisp programs
	An identiﬁer is represented as just a String
-}

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

{-
	Textually, S-expressions can optionally begin and end with any
	number of spaces; after throwing away leading and trailing spaces they
	consist of either an atom, or an open parenthesis followed by one or
	more S-expressions followed by a close parenthesis.

	atom ::= int
	| ident

	S ::= atom
	| (S*)

	the following are all valid S-expressions:
	5
	foo3
	(bar (foo) 3 5 874)
	(((lambda x (lambda y (plus x y))) 3) 5)
	( lots of ( spaces in ) this ( one ) )

	Write a parser for S-expressions
	Hints: To parse something but ignore its output, you can use the
	(*>) and (<*) operators, which have the types
	(*>) :: Applicative f => f a -> f b -> f b
	(<*) :: Applicative f => f a -> f b -> f a

	p1 *> p2 runs p1 and p2 in sequence, but ignores the result of
	p1 and just returns the result of p2. p1 <* p2 also runs p1 and p2 in
	sequence, but returns the result of p1 (ignoring p2’s result) instead.
	For example:
	*AParser> runParser (spaces *> posInt) " 345"
	Just (345,"")
-}

parseSExpr :: Parser SExpr
parseSExpr = error "todo"