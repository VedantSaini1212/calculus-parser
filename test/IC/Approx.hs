module IC.Approx ((~~>), (~~>?)) where

import Test.Tasty.HUnit (assertFailure, Assertion, HasCallStack)

import Control.Monad (unless)

epsilon :: Fractional a => a
epsilon = 1e-3

{-|
Asserts that the specified actual value is approximately equal to the
expected value (within some tolerance). The output message will contain
the prefix, the expected value, the actual value, and how much they are
allowed to vary.

If the prefix is the empty string (i.e., @\"\"@), then the prefix is omitted
and only the expected and actual values are output.
-}
assertApproxEqual :: (HasCallStack, Ord a, Num a, Show a)
                  => String -- ^ The message prefix
                  -> a      -- ^ The expected value
                  -> a      -- ^ The actual value
                  -> a      -- ^ The tolerance of error
                  -> Assertion
assertApproxEqual preface expected actual eps =
  unless (abs (expected - actual) < eps) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n")
           ++ "expected: " ++ show expected
           ++ "\n but got: " ++ show actual
           ++ "\n (floating values must be within: " ++ show eps ++ ")"

{-|
This function ensures that its first argument is roughly same as the second one.

This may mean that floating values are within a certain tolerance of each other,
or perhaps lists roughly the same elements (even if a different order).
-}
infix 1 ~~>
(~~>) :: (Show a, Ord a, Fractional a, HasCallStack)
      => a -- ^ the actual value
      -> a -- ^ the expected value
      -> Assertion
actual ~~> expected = assertApproxEqual "" expected actual epsilon

infix 1 ~~>?
(~~>?) :: (Show a, Ord a, Fractional a, HasCallStack)
      => Maybe a -- ^ the actual value
      -> Maybe a -- ^ the expected value
      -> Assertion
Nothing ~~>? Nothing = return ()
Just actual ~~>? Just expected = actual ~~> expected
actual ~~>? expected = assertFailure ("expected: " ++ show expected
                                   ++ "\n but got: " ++ show actual)
