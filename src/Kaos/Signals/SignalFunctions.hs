{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kaos.Signals.SignalFunctions
where

import Data.Proxy
import Kaos.Signals.TimeDomains
import Kaos.Signals.Linearity (Linearity(..), KnownLinearity, HasLinearity(..))
import qualified Kaos.Signals.Linearity as L
import Kaos.Signals.TimeDependence (TimeDependence(..), KnownTimeDependence, HasTimeDependence(..))
import qualified Kaos.Signals.TimeDependence as TD

type Time = Float

data SignalFunction (t :: TimeDomain) (l :: Linearity) (td :: TimeDependence) a b where
  Identity :: SignalFunction t 'Linear 'TimeInvariant a a
  Compose :: SignalFunction t l2 td2 b c -> SignalFunction t l1 td1 a b -> SignalFunction t (L.Compose l2 l1) (TD.Compose td2 td1) a c
  Constant :: a -> SignalFunction t 'Linear 'TimeInvariant () a
  Clock :: SignalFunction t 'Linear 'TimeVarying () Time
  Function :: (a -> b) -> SignalFunction t l 'TimeInvariant a b

instance (KnownTimeDomain t) => HasTimeDomain (SignalFunction t l td a b) where
  timeDomain _ = timeDomain (Proxy :: Proxy t)

instance (KnownLinearity l) => HasLinearity (SignalFunction t l td a b) where
  linearity _ = linearity (Proxy :: Proxy l)

instance (KnownTimeDependence td) => HasTimeDependence (SignalFunction t l td a b) where
  timeDependence _ = timeDependence (Proxy :: Proxy td)
