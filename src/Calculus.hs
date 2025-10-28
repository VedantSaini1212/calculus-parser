module Calculus (sumTo, vars, inTermsOf, eval, pretty, diff, maclaurin, pi) where

import Prelude hiding (pi)

import Expr

import Data.Map (Map, (!))
import Data.Map qualified as Map

import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Base (VecElem(DoubleElemRep))
import GHC.Float (fromRat, int2Double)

type Env = Map String Double

---------------------------------------------------------------------------
-- Type classes and class instances

instance Num Expr where
  fromInteger :: Integer -> Expr
  fromInteger n = Val (fromInteger n)
  negate      :: Expr -> Expr
  negate      = Pre Neg
  (+), (*)    :: Expr -> Expr -> Expr
  (+)         = Bin Add
  (*)         = Bin Mul

instance Fractional Expr where
  fromRational :: Rational -> Expr
  fromRational n = Val (fromRational n)
  (/)          :: Expr -> Expr -> Expr
  (/)          = Bin Div

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
pretty :: Expr -> String
pretty = show

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
diff x (Pre Sin expr) = cos expr * diff x expr
diff x (Pre Cos expr) = -(sin expr * diff x expr)
diff x (Pre Log expr) = diff x expr / expr
diff x (Pre Exp (Id var)) = if x == var then exp (Id var) else Val 0
diff x (Pre Exp expr) = diff x expr * exp expr
diff x (Pre Neg expr) = -(diff x expr)


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
      rest = zipWith (/) (zipWith (*) fs xs) facs
        where
          fs = map (eval (Map.fromList [("x", 0)])) (iterate (diff "x") expr)
          xs = [x^m | m <- [0..]]
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
  fromInteger  = undefined
  negate       :: [a] -> [a]
  negate       = undefined
  (+), (*)     :: [a] -> [a] -> [a]
  (+)          = undefined
  (*)          = undefined

instance Fractional a => Fractional [a] where
  fromRational :: Rational -> [a]
  fromRational = undefined
  (/)          :: [a] -> [a] -> [a]
  (/)          = undefined

instance Floating a => Floating [a] where
  (**)         :: [a] -> [a] -> [a]
  (**)         = undefined

pi :: (Enum a, Fractional a) => Int -> a
pi = undefined
