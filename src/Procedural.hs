{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

module Procedural where

import Control.Monad.State
import Control.Lens

data Contract a
  = SendOne Horizon (Contract a)
  | ReceiveOne Horizon (Contract a)
  | Scale Double (Contract a)
  | Observe (Double -> Contract a)
  | GetTime (Time -> Contract a)
  | Choose (Bool -> Contract a)
  | After Time (Contract a)
  | And (Contract a) (Contract a)
  | Pure a
  | Fail
  deriving (Functor)

type Amount = Double
type Time = Int
type Scale = Double
data Horizon = Time Time | Infinite deriving (Eq, Ord)

sendOne :: Contract ()
sendOne = SendOne Infinite (Pure ())

receiveOne :: Contract ()
receiveOne = ReceiveOne Infinite (Pure ())

scale :: Double -> Contract ()
scale c = Scale c (Pure ())

observe :: Contract Double
observe = Observe Pure

getTime :: Contract Time
getTime = GetTime Pure

choose :: Contract Bool
choose = Choose Pure

after :: Time -> Contract ()
after t = After t (Pure ())

instance Applicative Contract where
  pure = Pure
  pf <*> a = recur pf where
    recur (Pure f) = fmap f a
    recur (SendOne h cont) = SendOne h (recur cont)
    recur (ReceiveOne h cont) = ReceiveOne h (recur cont)
    recur (Scale c cont) = Scale c (recur cont)
    recur (Observe cont) = Observe (recur . cont)
    recur (GetTime cont) = GetTime (recur . cont)
    recur (Choose cont) = Choose (recur . cont)
    recur (After t cont) = After t (recur cont)
    recur Fail = Fail

instance Monad Contract where
  return = pure
  ma >>= f = recur ma where
    recur (Pure a) = f a
    recur (SendOne h cont) = SendOne h (recur cont)
    recur (ReceiveOne h cont) = ReceiveOne h (recur cont)
    recur (Scale c cont) = Scale c (recur cont)
    recur (Observe cont) = Observe (recur . cont)
    recur (GetTime cont) = GetTime (recur . cont)
    recur (Choose cont) = Choose (recur . cont)
    recur (After t cont) = After t (recur cont)
    recur Fail = Fail

getHorizon :: Contract a -> Horizon
getHorizon (Pure a) = Infinite
getHorizon (SendOne h cont) = h
getHorizon (ReceiveOne h cont) = h
getHorizon (Scale c cont) = Infinite
getHorizon (Observe cont) = Infinite
getHorizon (GetTime cont) = Infinite
getHorizon (Choose cont) = Infinite
getHorizon (After t cont) = Infinite
getHorizon Fail = Infinite

setHorizon :: Horizon -> Contract a -> Contract a
setHorizon h' (SendOne h cont) = SendOne h' cont
setHorizon h' (ReceiveOne h cont) = ReceiveOne h' cont
setHorizon h' c = c

zcb :: Double -> Time -> Time -> Contract ()
zcb amt t1 t2 = do
  scale amt
  after t1
  setHorizon (Time t2) (sendOne)

data Balances = Balances { _party :: Double, _counterparty :: Double } deriving Show
makeLenses ''Balances

defaultBalances = Balances 0.0 0.0
simpleContract = do
  after 0
  scale 5.0
  sendOne
  scale 0.5
  receiveOne
executionResult = runState (interpret (1.0, 0) simpleContract) defaultBalances

interpret :: (Scale, Time) -> Contract a -> State Balances (Maybe a)
interpret (s,t) (Pure a) = pure (Just a)
interpret (s,t) (SendOne h cont) = do
  (%=) party (\x -> x - s)
  (%=) counterparty (s +)
  interpret (s,t) cont
interpret (s,t) (ReceiveOne h cont) = do
  (%=) counterparty (\x -> x - s)
  (%=) party (s +)
  interpret (s,t) cont
interpret (s,t) (Scale c cont) = interpret (s*c,t) cont
interpret (s,t) (Observe cont) = interpret (s,t) (cont 2.0)
interpret (s,t) (GetTime cont) = interpret (s,t) (cont 1)
interpret (s,t) (Choose cont) = interpret (s,t) (cont True)
interpret (s,t) (After t' cont) = if t >= t' then interpret (s,t) cont else interpret (s,t) Fail
interpret (s,t) Fail = pure Nothing