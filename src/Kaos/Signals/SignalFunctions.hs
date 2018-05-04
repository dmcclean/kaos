{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kaos.Signals.SignalFunctions
where

import Prelude hiding ((.), id, init)
import Data.Proxy
import Kaos.Signals.TimeDomains
import Kaos.Signals.Linearity (Linearity(..), KnownLinearity, HasLinearity(..))
import Kaos.Signals.TimeDependence (TimeDependence(..), KnownTimeDependence, HasTimeDependence(..))
import Text.LaTeX
import qualified Text.LaTeX.Packages.AMSMath as AMS
import Control.Category
import Control.Arrow

type Time = Float

class ArrowLoop k => ArrowInit k where
  init :: a -> k a a

class Arrow k => ArrowTime k where
  clock :: k () Time

data SignalFunction (t :: TimeDomain) (l :: Linearity) (td :: TimeDependence) a b where
  Identity :: SignalFunction t l td a a
  Compose :: SignalFunction t l td b c -> SignalFunction t l td a b -> SignalFunction t l td a c
  Constant :: LaTeX -> a -> SignalFunction t l td () a
  Clock :: SignalFunction t l 'TimeVarying () Time
  UnaryFunction :: LaTeX -> (a -> b) -> SignalFunction t l td a b
  Init :: LaTeX -> a -> SignalFunction t l td a a
  DiscreteDelay :: LaTeX -> a -> SignalFunction 'DiscreteTime l td a a
  Derivative :: (Num a) => SignalFunction 'ContinuousTime l td a a -- TODO: dimensional type
  Integral :: (Num a) => SignalFunction 'ContinuousTime l td a a
  Loop :: SignalFunction t l td (a, c) (b, c) -> SignalFunction t l td a b
  First :: SignalFunction t l td a b -> SignalFunction t l td (a, c) (b, c)

interpret :: (ArrowInit k, ArrowTime k) => SignalFunction t l td a b -> k a b
interpret Identity = id
interpret (Compose g f) = interpret g . interpret f
interpret (Constant _ x) = arr $ const x
interpret Clock = clock
interpret (UnaryFunction _ f) = arr f
interpret (Init _ x0) = init x0
interpret (DiscreteDelay _ x0) = init x0
interpret Derivative = undefined
interpret Integral = undefined
interpret (Loop f) = loop $ interpret f
interpret (First f) = first $ interpret f

interpretTI :: (ArrowInit k) => SignalFunction t l 'TimeInvariant a b -> k a b
interpretTI Identity = id
interpretTI (Compose g f) = interpretTI g . interpretTI f
interpretTI (Constant _ x) = arr $ const x
interpretTI (UnaryFunction _ f) = arr f
interpretTI (Init _ x0) = init x0
interpretTI (DiscreteDelay _ x0) = init x0
interpretTI Derivative = undefined
interpretTI Integral = undefined
interpretTI (Loop f) = loop $ interpretTI f
interpretTI (First f) = first $ interpretTI f

instance Category (SignalFunction t l td) where
  id = Identity
  (.) = Compose

sin :: (Floating a) => SignalFunction t 'NonLinear td a a
sin = UnaryFunction AMS.tsin Prelude.sin

cos :: (Floating a) => SignalFunction t 'NonLinear td a a
cos = UnaryFunction AMS.tcos Prelude.cos

tan :: (Floating a) => SignalFunction t 'NonLinear td a a
tan = UnaryFunction AMS.ttan Prelude.tan

csc :: (Floating a) => SignalFunction t 'NonLinear td a a
csc = UnaryFunction AMS.csc (Prelude.recip . Prelude.sin)

sec :: (Floating a) => SignalFunction t 'NonLinear td a a
sec = UnaryFunction AMS.sec (Prelude.recip . Prelude.cos)

cot :: (Floating a) => SignalFunction t 'NonLinear td a a
cot = UnaryFunction AMS.cot (Prelude.recip . Prelude.tan)

exp :: (Floating a) => SignalFunction t 'NonLinear td a a
exp = UnaryFunction (AMS.math $ texy "e^x") (Prelude.exp)

instance (KnownTimeDomain t) => HasTimeDomain (SignalFunction t l td a b) where
  timeDomain _ = timeDomain (Proxy :: Proxy t)

instance (KnownLinearity l) => HasLinearity (SignalFunction t l td a b) where
  linearity _ = linearity (Proxy :: Proxy l)

instance (KnownTimeDependence td) => HasTimeDependence (SignalFunction t l td a b) where
  timeDependence _ = timeDependence (Proxy :: Proxy td)
