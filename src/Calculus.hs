module Calculus (sumTo, vars, inTermsOf, eval, pretty, diff, maclaurin, pi) where

import Prelude hiding (pi)

import Expr

import Data.Map (Map, (!))
import Data.Map qualified as Map

import Data.Set (Set)
import Data.Set qualified as Set

type Env = Map String Double

---------------------------------------------------------------------------
-- Type classes and class instances

instance Num Expr where
  fromInteger :: Integer -> Expr
  fromInteger = Val . fromInteger 

  negate :: Expr -> Expr
  negate (Val 0) = 0
  negate e = Pre Neg e

  (*) :: Expr -> Expr -> Expr
  Val 0 * _ = 0
  _ * Val 0 = 0
  Val 1 * e = e
  e * Val 1 = e 
  e1 * e2 = Bin Mul e1 e2

  (+) :: Expr -> Expr -> Expr
  Val 0 + e = e
  e + Val 0 = e
  e1 + e2 = Bin Add e1 e2

instance Fractional Expr where
  fromRational :: Rational -> Expr
  fromRational = Val . fromRational
  (/) :: Expr -> Expr -> Expr
  Val 0 / e = 0
  e / Val 1 = e
  e1/e2 = Bin Div e1 e2

instance Floating Expr where
  sin, cos, log, exp :: Expr -> Expr
  sin = Pre Sin
  cos = Pre Cos
  log = Pre Log
  exp = Pre Exp

---------------------------------------------------------------------------

{-|
Computes the sum of first `n` terms of the given series.
-}
sumTo :: Num a => Int -> [a] -> a
sumTo n = sum . take n

{-|
Computes the set of variable names that appear within the given expression.
-}
vars :: Expr -> Set String
vars (Val _) = Set.empty
vars (Id x) = Set.singleton x
vars (Bin _ e1 e2) = Set.union (vars e1) (vars e2)
vars (Pre _  e1) = vars e1

{-|
Does the given expression only use the given variable and no others?
-}
inTermsOf :: String -> Expr -> Bool
inTermsOf ch = (`Set.isSubsetOf` Set.singleton ch) . vars

{-|
Evaluates a given expression, evaluating any variables to their value within
the provided environment.
-}
binOps :: Map BinOp (Double -> Double -> Double)
binOps = Map.fromList [(Add, (+)), (Mul, (*)), (Div, (/))]

preOps :: Map PreOp (Double -> Double)
preOps = Map.fromList [(Neg, negate), (Sin,sin), (Cos,cos), (Log, log), (Exp, exp)]

eval :: Env -> Expr -> Double
eval maps (Val x) = x
eval maps (Id x) = maps ! x
eval maps (Bin op e1 e2) = f (eval maps e1) (eval maps e2) where f = binOps ! op
eval maps (Pre op e) = f (eval maps e) where f = preOps ! op


{-| OPTIONAL
Pretty prints an expression to a more human-readable form.
-}
preSymbol :: PreOp -> String
preSymbol Sin = "sin"
preSymbol Cos = "cos"
preSymbol Log = "log"
preSymbol Exp = "e^"
preSymbol _ = "-"
binSymbol :: BinOp -> String
binSymbol Add = "+"
binSymbol Mul = "*"
binSymbol Div = "/"

pretty :: Expr -> String
pretty (Val x) = show x
pretty (Id x)  = x
pretty (Bin Add e1 (Pre Neg e2)) =
    "(" ++ pretty e1 ++ "-" ++ pretty e2 ++ ")"
pretty (Bin op e1 e2) =
    "(" ++ pretty e1 ++ binSymbol op ++ pretty e2 ++ ")"
pretty (Pre Neg e) = "-(" ++ pretty e ++ ")"
pretty (Pre op e) =
    preSymbol op ++ "(" ++ pretty e ++ ")"


{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: String -> Expr -> Expr
diff x (Id var)
  | x == var = Val 1
  | otherwise = Val 0
diff _ (Val _) = Val 0
diff x (Bin Add e1 e2) = diff x e1 + diff x e2
diff x (Bin Mul e1 e2) = e1 * diff x e2  + diff x e1 * e2
diff x (Bin Div e1 e2) = (diff x e1 * e2 - e1 * diff x e2 ) / (e2 ^ 2)
diff x (Pre Sin e) = cos e * diff x e
diff x (Pre Cos e) = -(sin e * diff x e)
diff x (Pre Log e) = diff x e / e
diff x (Pre Exp e) = diff x e * exp e
diff x (Pre Neg e) = -(diff x e)


{-|
Computes the approximation of an expression `f` by expanding the Maclaurin
series on `f` and taking its summation.
-}
maclaurin :: Expr   -- ^ expression to approximate (with `x` free)
          -> Double -- ^ value to give to `x`
          -> Int    -- ^ number of terms to expand
          -> Maybe Double -- ^ the approximate result
maclaurin expr x n 
  | vars expr /= Set.singleton "x" = Nothing
  | otherwise = Just (sumTo n rest)
    where
      rest = zipWith3 (\f i k -> f * (x ** i) / k)
                fs [0..] facs
        where
          fs = map (eval (Map.fromList [("x", 0)])) (iterate (diff "x") expr)
          facs = scanl (*) 1 [1..]

-- Recursive method
{-|
maclaurin expr x n 
  | vars expr /= Set.singleton "x" = Nothing
  | otherwise = Just (sumTo n (go expr x (fromIntegral n)))
  where
    go :: Expr -> Double -> Double -> [Double]
    go (Val num) _ _= [num]
    go expr x n1 = (x**inc * eval (Map.fromList[("x",0)]) expr/product [1..inc]) : go (diff "x" expr) x (n1-1)
      where inc = fromIntegral n-n1
   -}
    

-- Extension
instance Num a => Num [a] where
  fromInteger  :: Integer -> [a]
  fromInteger x = repeat (fromInteger x)
  negate       :: [a] -> [a]
  negate       = map negate 
  (+),(*) :: [a] -> [a] -> [a]
  (+) = zipWith (+) 
  (*) = zipWith (*)
  
instance Fractional a => Fractional [a] where
  fromRational :: Rational -> [a]
  fromRational x = repeat (fromRational x)
  (/)          :: [a] -> [a] -> [a]
  (/)          = zipWith (/)

instance Floating a => Floating [a] where
  (**)         :: [a] -> [a] -> [a]
  (**)         = zipWith (**)

pi :: (Enum a, Fractional a) => Int -> a
pi n = sumTo n (zipWith (/) num den)
  where
    num = cycle [4,-4]
    den = enumFromThen 1 3
