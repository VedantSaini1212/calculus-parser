module Expr where

{-|
This is a datatyping representing arithmetic expressions.
-}
data Expr = Val Double | Id String
          | Bin BinOp Expr Expr
          | Pre PreOp Expr
          deriving Show

data BinOp = Add | Mul | Div deriving (Eq, Ord, Show)
data PreOp = Neg | Sin | Log | Cos | Exp deriving (Eq, Ord, Show)

{- Test cases from the spec. -}
e1, e2, e3, e4, e5, e6 :: Expr

-- | 5*x
e1 =  Bin Mul (Val 5.0) (Id "x")

-- | x*x + y - 7
e2 =  Bin Add (Bin Add (Bin Mul (Id "x") (Id "x"))
                       (Id "y"))
              (Pre Neg (Val 7.0))

-- | x-y^2/(4*x*y-y^2)::Expr
e3 =  Bin Add (Id "x")
              (Pre Neg (Bin Div (Bin Mul (Id "y") (Id "y"))
                       (Bin Add (Bin Mul (Bin Mul (Val 4.0) (Id "x"))
                                         (Id "y"))
                                (Pre Neg (Bin Mul (Id "y") (Id "y"))))))

-- | -cos x :: Expr
e4 = Pre Neg (Pre Cos (Id "x"))

-- | sin (1+log(2*x)) :: Expr
e5 = Pre Sin (Bin Add (Val 1.0)
                      (Pre Log (Bin Mul (Val 2.0) (Id "x"))))

-- | log(3*x^2+2) :: Expr
e6 = Pre Log (Bin Add (Bin Mul (Val 3.0)
                               (Bin Mul (Id "x") (Id "x")))
                      (Val 2.0))
