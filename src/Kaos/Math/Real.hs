module Kaos.Math.Real
where

-- This is a numeric type which is implemented in Haskell using floating point numbers, but which is held
-- abstract from the perspective of the DSL definitions. The intent is that DSL compilers may elect to use
-- different physical representations (fixed point, low precision floating point) as it determines to be
-- appropriate based on signal characteristics.
-- Instances aren't provided for the Haskell numeric class heirarchy because we want to expose tighter semantics
-- on the DSLs.
newtype Real = Real Double
  deriving (Eq, Ord, Read, Show)
