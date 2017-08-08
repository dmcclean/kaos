{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Kaos.Signals.TimeDependence
where

import Data.Proxy

data TimeDependence = TimeInvariant
                    | TimeVarying
  deriving (Eq, Show, Read)

class HasTimeDependence a where
  timeDependence :: a -> TimeDependence

instance HasTimeDependence TimeDependence where
  timeDependence = id

type KnownTimeDependence (td :: TimeDependence) = HasTimeDependence (Proxy td)

instance HasTimeDependence (Proxy 'TimeInvariant) where
  timeDependence _ = TimeInvariant

instance HasTimeDependence (Proxy 'TimeVarying) where
  timeDependence _ = TimeVarying

type family Compose (a :: TimeDependence) (b :: TimeDependence) where
  Compose TimeInvariant TimeInvariant = TimeInvariant
  Compose TimeVarying TimeInvariant = TimeVarying
  Compose TimeInvariant TimeVarying = TimeVarying
  Compose TimeVarying TimeVarying = TimeVarying
