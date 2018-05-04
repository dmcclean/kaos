{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Kaos.Signals.Linearity
where

import Data.Proxy

data Linearity = Linear
               | NonLinear
  deriving (Eq, Show, Read)

class HasLinearity a where
  linearity :: a -> Linearity

instance HasLinearity Linearity where
  linearity = id

type KnownLinearity (l :: Linearity) = HasLinearity (Proxy l)

instance HasLinearity (Proxy 'Linear) where
  linearity _ = Linear

instance HasLinearity (Proxy 'NonLinear) where
  linearity _ = NonLinear

type family Compose (a :: Linearity) (b :: Linearity) where
  Compose Linear NonLinear = NonLinear
  Compose NonLinear Linear = NonLinear
  Compose a a = a
