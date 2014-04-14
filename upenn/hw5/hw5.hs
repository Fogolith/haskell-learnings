{-# LANGUAGE FlexibleInstances #-}
-- http://www.seas.upenn.edu/~cis194/hw/05-type-classes.pdf
import ExprT (ExprT(..))
import Parser (parseExp)
import qualified StackVM as VM

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

instance Expr VM.Program where
	add a b = a ++ b ++ [VM.Add]
	mul a b = a ++ b ++ [VM.Mul]
	lit i = [VM.PushI i]

compile :: String -> Maybe VM.Program
compile s = parseExp lit add mul s

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr = evalMaybe . parseExp Lit Add Mul

evalMaybe :: Maybe ExprT -> Maybe Integer
evalMaybe (Just y) = Just (eval y)
evalMaybe _ = Nothing

