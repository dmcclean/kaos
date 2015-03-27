{-# LANGUAGE RankNTypes #-}

module Kaos.Math.AugmentedRational
where

import Data.Group
import Data.Monoid

data AugmentedRational = Exact Integer Rational
                       | Approximate (forall a.Floating a => a)

exact :: Integer -> Rational -> AugmentedRational
exact z q | q == 0 = Exact 0 0
          | otherwise = Exact z q

instance Show AugmentedRational where
  show (Exact z q) | z == 0 = "Exactly " ++ show q
                   | z == 1 = "Exactly pi * " ++ show q
                   | otherwise = "Exactly pi^" ++ show z ++ " * " ++ show q
  show (Approximate x) = "Approximately " ++ show (x :: Double)

instance Num AugmentedRational where
  fromInteger n = exact 0 (fromInteger n)
  (Exact z1 q1) * (Exact z2 q2) = exact (z1 + z2) (q1 * q2)
  x * y = Approximate $ approximateValue x * approximateValue y
  (Exact z1 q1) + (Exact z2 q2) | z1 == z2 = exact z1 (q1 + q2) -- by distributive property
  x + y = Approximate $ approximateValue x + approximateValue y
  abs (Exact z q) = exact z (abs q)
  signum (Exact _ q) = exact 0 (signum q)
  signum (Approximate x) = Approximate $ signum x
  negate x = (-1) * x

instance Fractional AugmentedRational where
  fromRational = exact 0
  recip (Exact z q) = exact z (recip q)

instance Floating AugmentedRational where
  pi = exact 1 1
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

approximateValue :: Floating a => AugmentedRational -> a
approximateValue (Exact z q) = (pi ** (fromInteger z)) * (fromRational q)
approximateValue (Approximate x) = x

approx1 :: (forall a.Floating a => a -> a) -> AugmentedRational -> AugmentedRational
approx1 f x = Approximate (f (approximateValue x))

approx1' :: [((Integer, Rational), (Integer, Rational))] -> (forall a.Floating a => a -> a) -> AugmentedRational -> AugmentedRational
approx1' exacts f x@(Exact z q) = let exactResult = lookup (z,q) exacts
                                   in case exactResult of
                                        Nothing -> approx1 f x
                                        Just (z', q') -> exact z' q'
approx1' _ f x = approx1 f x

instance Monoid AugmentedRational where
  mempty = 1
  mappend = (*)

instance Group AugmentedRational where
  invert = recip

instance Abelian AugmentedRational