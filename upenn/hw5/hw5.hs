{-# LANGUAGE TypeSynonymInstances #-}
-- http://www.seas.upenn.edu/~cis194/hw/05-type-classes.pdf
import ExprT (ExprT(..))
import Parser (parseExp)
import StackVM

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class Expr a where
	lit :: Integer -> a
	add :: a -> a -> a
	mul :: a -> a -> a

instance Expr ExprT where
	lit x = Lit x
	add x y = Add x y
	mul x y = Mul x y

instance Expr Integer where
	lit = id
	add = (+)
	mul = (*)

instance Expr Bool where
	lit x = x > 0
	add = (||)
	mul = (&&)

instance Expr MinMax where
	add (MinMax a) (MinMax b) = MinMax $ max a b
	mul (MinMax a) (MinMax b) = MinMax $ min a b
	lit a = MinMax a

instance Expr Mod7 where
	lit x = Mod7 (mod x 7)
	add (Mod7 a) (Mod7 b) = lit (a + b)
	mul (Mod7 a) (Mod7 b) = lit (a * b)


{-
Simply create an instance of the Expr type class for Program, so that
arithmetic expressions can be interpreted as compiled programs. For
any arithmetic expression exp :: Expr a => a it should be the case
that

stackVM exp == Right [IVal exp]

then , put together the pieces you have to create a function
, put together the pieces you have to create a function -}

compile :: String -> Maybe Program
compile = error "todo"

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

reify :: ExprT -> ExprT
reify = id

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr = evalMaybe . parseExp Lit Add Mul

evalMaybe :: Maybe ExprT -> Maybe Integer
evalMaybe (Just y) = Just (eval y)
evalMaybe _ = Nothing

