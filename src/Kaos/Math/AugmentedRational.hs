{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Kaos.Math.AugmentedRational
Description : Exact rational multiples of powers of pi
License     : MIT
Maintainer  : douglas.mcclean@gmail.com
Stability   : experimental

This type is sufficient to exactly express the closure of Q ∪ {π} under multiplication and division.
As a result it is useful for representing conversion factors
between physical units. Approximate values are included both to close the remainder
of the arithmetic operations in the `Num` typeclass and to encode conversion
factors defined experimentally.
-}
module Kaos.Math.AugmentedRational
(
  AugmentedRational(Approximate),
  pattern Exact,
  approximateValue
)
where

import Data.Group

-- | Represents an exact or approximate real value.
-- The exactly representable values are rational multiples of an integer power of pi.
data AugmentedRational = Exact' Integer Rational
                       | Approximate (forall a.Floating a => a) -- ^ An approximate value.

-- | Matches values which are exactly known, or constructs such a value from
-- an integer power of pi and a rational factor.
pattern Exact z q <- Exact' z q where
  Exact z q | q == 0 = Exact' 0 0
            | otherwise = Exact' z q

-- | Approximates an exact or approximate value, converting it to a `Floating` type.
-- This uses the value of `pi` supplied by the destination type, to provide the appropriate
-- precision.
approximateValue :: Floating a => AugmentedRational -> a
approximateValue (Exact z q) = (pi ** (fromInteger z)) * (fromRational q)
approximateValue (Approximate x) = x

instance Show AugmentedRational where
  show (Exact z q) | z == 0 = "Exactly " ++ show q
                   | z == 1 = "Exactly pi * " ++ show q
                   | otherwise = "Exactly pi^" ++ show z ++ " * " ++ show q
  show (Approximate x) = "Approximately " ++ show (x :: Double)

instance Num AugmentedRational where
  fromInteger n = Exact 0 (fromInteger n)
  (Exact z1 q1) * (Exact z2 q2) = Exact (z1 + z2) (q1 * q2)
  (Exact _ 0) * _ = 0
  _ * (Exact _ 0) = 0
  x * y = Approximate $ approximateValue x * approximateValue y
  (Exact z1 q1) + (Exact z2 q2) | z1 == z2 = Exact z1 (q1 + q2) -- by distributive property
  x + y = Approximate $ approximateValue x + approximateValue y
  abs (Exact z q) = Exact z (abs q)
  abs (Approximate x) = Approximate $ abs x
  signum (Exact _ q) = Exact 0 (signum q)
  signum (Approximate x) = Approximate $ signum x -- we leave this tagged as approximate because we don't know "how" approximate the input was. a case could be made for exact answers here.
  negate x = (-1) * x

instance Fractional AugmentedRational where
  fromRational = Exact 0
  recip (Exact z q) = Exact z (recip q)

instance Floating AugmentedRational where
  pi = Exact 1 1
  exp = approx1' [((0, 0), (0, 1))] exp
  log = approx1' [((0, 1), (0, 0))] log
  sin = approx1' [((0, 0), (0, 0)),
                  ((1, 1/2), (0, 1)),
                  ((1, 1), (0, 0))] sin
  cos = sin . (+ (pi / 2))
  tan = approx1 tan
  asin = approx1 asin
  atan = approx1 atan
  acos = approx1 acos
  sinh = approx1 sinh
  cosh = approx1 cosh
  tanh = approx1 tanh
  asinh = approx1 asinh
  acosh = approx1 acosh
  atanh = approx1 atanh

approx1 :: (forall a.Floating a => a -> a) -> AugmentedRational -> AugmentedRational
approx1 f x = Approximate (f (approximateValue x))

approx1' :: [((Integer, Rational), (Integer, Rational))] -> (forall a.Floating a => a -> a) -> AugmentedRational -> AugmentedRational
approx1' exacts f x@(Exact z q) = let exactResult = lookup (z,q) exacts
                                   in case exactResult of
                                        Nothing -> approx1 f x
                                        Just (z', q') -> Exact z' q'
approx1' _ f x = approx1 f x

-- | The multiplicative monoid over augmented rationals.
instance Monoid AugmentedRational where
  mempty = 1
  mappend = (*)

-- | The multiplicative group over augmented rationals.
instance Group AugmentedRational where
  invert = recip

instance Abelian AugmentedRational