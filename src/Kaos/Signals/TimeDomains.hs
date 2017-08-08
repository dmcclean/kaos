{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Kaos.Signals.TimeDomains
where

import Data.Proxy

data TimeDomain = ContinuousTime
                | DiscreteTime -- some representation of tick interval
  deriving (Eq, Show, Read)

class HasTimeDomain a where
  timeDomain :: a -> TimeDomain

instance HasTimeDomain TimeDomain where
  timeDomain = id

type KnownTimeDomain (t :: TimeDomain) = HasTimeDomain (Proxy t)

instance HasTimeDomain (Proxy 'ContinuousTime) where
  timeDomain _ = ContinuousTime

instance HasTimeDomain (Proxy 'DiscreteTime) where
  timeDomain _ = DiscreteTime
