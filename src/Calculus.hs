module Calculus (vars, inTermsOf, eval, pretty, diff, maclaurin, pi) where

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
  fromInteger = undefined
  negate      :: Expr -> Expr
  negate      = undefined
  (+), (*)    :: Expr -> Expr -> Expr
  (+)         = undefined
  (*)         = undefined

instance Fractional Expr where
  fromRational :: Rational -> Expr
  fromRational = undefined
  (/)          :: Expr -> Expr -> Expr
  (/)          = undefined

instance Floating Expr where
  sin, cos, log, exp :: Expr -> Expr
  sin = undefined
  cos = undefined
  log = undefined
  exp = undefined

---------------------------------------------------------------------------

{-|
Computes the sum of first `n` terms of the given series.
-}
sumTo :: Num a => Int -> [a] -> a
sumTo = undefined

{-|
Computes the set of variable names that appear within the given expression.
-}
vars :: Expr -> Set String
vars = undefined

{-|
Does the given expression only use the given variable and no others?
-}
inTermsOf :: String -> Expr -> Bool
inTermsOf = undefined

{-|
Evaluates a given expression, evaluating any variables to their value within
the provided environment.
-}
eval :: Expr -> Env -> Double
eval = undefined

{-| OPTIONAL
Pretty prints an expression to a more human-readable form.
-}
pretty :: Expr -> String
pretty = show

{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: Expr -> String -> Expr
diff = undefined

{-|
Computes the approximation of an expression `f` by expanding the Maclaurin
series on `f` and taking its summation.
-}
maclaurin :: Expr   -- ^ expression to approximate (with `x` free)
          -> Double -- ^ value to give to `x`
          -> Int    -- ^ number of terms to expand
          -> Double -- ^ the approximate result
maclaurin = undefined

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
