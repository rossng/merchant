{-# LANGUAGE TypeOperators #-}
module Horizon where

import Language
import ALaCarte

horizon :: Contract -> Horizon
horizon = handle (const Infinite) horizonAlg

class Functor f => HorizonAlg f where
  horizonAlg :: f Horizon -> Horizon

instance HorizonAlg ContractF where
  horizonAlg Zero = Infinite
  horizonAlg (One k) = Infinite
  horizonAlg (Give c) = c
  horizonAlg (And c1 c2) = max c1 c2
  horizonAlg (Or c1 c2) = max c1 c2
  horizonAlg (Scale o c) = c

instance HorizonAlg OriginalF where
  horizonAlg (Truncate t c) = min (Time t) c
  horizonAlg (Then c1 c2) = max c1 c2
  horizonAlg (Get c) = c
  horizonAlg (Anytime c) = c

instance (HorizonAlg f, HorizonAlg g) => HorizonAlg (f :+ g) where
  renderAlg (L x) = renderAlg x
  renderAlg (R y) = renderAlg y