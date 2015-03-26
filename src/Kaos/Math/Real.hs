{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kaos.Math.Real
where

import Prelude hiding (Real)
import qualified Prelude as P

-- This is a numeric type which is implemented in Haskell using floating point numbers, but which is held
-- abstract from the perspective of the DSL definitions. The intent is that DSL compilers may elect to use
-- different physical representations (fixed point, low precision floating point) as it determines to be
-- appropriate based on signal characteristics.
newtype Real = Real Double
  deriving (Eq, Ord, Read, Show, Num, Fractional, Floating, P.Real, RealFrac, RealFloat)