module Kaos.Math.FinallyTagless
where

import Prelude hiding (Fractional(..), Floating(..))
import Prelude (Fractional, Floating)
import qualified Prelude as P
import Control.Applicative
import Data.Functor.Identity

class Numericliteral e where
  fromRational :: (Fractional x) => Rational -> e x

class Trigonometry e where
  sin :: (Floating x) => e x -> e x
  cos :: (Floating x) => e x -> e x
  tan :: (Floating x) => e x -> e x
  csc :: (Floating x) => e x -> e x
  sec :: (Floating x) => e x -> e x
  cot :: (Floating x) => e x -> e x
  asin :: (Floating x) => e x -> e x
  acos :: (Floating x) => e x -> e x
  atan :: (Floating x) => e x -> e x
  acsc :: (Floating x) => e x -> e x
  asec :: (Floating x) => e x -> e x
  acot :: (Floating x) => e x -> e x
  atan2 :: (RealFloat x) => e x -> e x -> e x

instance Numericliteral Identity where
  fromRational = Identity . P.fromRational

instance Trigonometry Identity where
  sin = fmap P.sin
  cos = fmap P.cos
  tan = fmap P.tan
  csc = fmap (P.recip . P.sin)
  sec = fmap (P.recip . P.cos)
  cot = fmap (P.recip . P.tan)
  asin = fmap P.asin
  acos = fmap P.acos
  atan = fmap P.atan
  acsc = fmap (P.asin . P.recip)
  asec = fmap (P.acos . P.recip)
  acot = fmap (P.atan . P.recip)
  atan2 = liftA2 P.atan2