{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

module Procedural where

import Control.Monad.State
import Control.Lens

data Contract a
  = SendOne Horizon (Contract a)
  | ReceiveOne Horizon (Contract a)
  | Scale Horizon Double (Contract a)
  | Observe Horizon (Double -> Contract a)
  | GetTime Horizon (Time -> Contract a)
  | Choose Horizon (Bool -> Contract a)
  | Pure a

type Time = Int
data Horizon = Time Time | Infinite

sendOne :: Contract ()
sendOne = SendOne Infinite (Pure ())

receiveOne :: Contract ()
receiveOne = ReceiveOne Infinite (Pure ())

scale :: Double -> Contract ()
scale c = Scale Infinite c (Pure ())

observe :: Contract Double
observe = Observe Infinite Pure

getTime :: Contract Time
getTime = GetTime Infinite Pure

choose :: Contract Bool
choose = Choose Infinite Pure

instance Functor Contract where
  fmap f = recur where
    recur (Pure a) = Pure (f a)
    recur (SendOne h cont) = SendOne h (recur cont)
    recur (ReceiveOne h cont) = ReceiveOne h (recur cont)
    recur (Scale h c cont) = Scale h c (recur cont)
    recur (Observe h cont) = Observe h (recur . cont)
    recur (GetTime h cont) = GetTime h (recur . cont)
    recur (Choose h cont) = Choose h (recur . cont)

instance Applicative Contract where
  pure = Pure
  pf <*> a = recur pf where
    recur (Pure f) = fmap f a
    recur (SendOne h cont) = SendOne h (recur cont)
    recur (ReceiveOne h cont) = ReceiveOne h (recur cont)
    recur (Scale h c cont) = Scale h c (recur cont)
    recur (Observe h cont) = Observe h (recur . cont)
    recur (GetTime h cont) = GetTime h (recur . cont)
    recur (Choose h cont) = Choose h (recur . cont)

instance Monad Contract where
  return = pure
  ma >>= f = recur ma where
    recur (Pure a) = f a
    recur (SendOne h cont) = SendOne h (recur cont)
    recur (ReceiveOne h cont) = ReceiveOne h (recur cont)
    recur (Scale h c cont) = Scale h c (recur cont)
    recur (Observe h cont) = Observe h (recur . cont)
    recur (GetTime h cont) = GetTime h (recur . cont)
    recur (Choose h cont) = Choose h (recur . cont)

getHorizon :: Contract a -> Horizon
getHorizon (Pure a) = Infinite
getHorizon (SendOne h cont) = h
getHorizon (ReceiveOne h cont) = h
getHorizon (Scale h c cont) = h
getHorizon (Observe h cont) = h
getHorizon (GetTime h cont) = h
getHorizon (Choose h cont) = h

setHorizon :: Horizon -> Contract a -> Contract a
setHorizon h' (Pure a) = Pure a
setHorizon h' (SendOne h cont) = SendOne h' cont
setHorizon h' (ReceiveOne h cont) = ReceiveOne h' cont
setHorizon h' (Scale h c cont) = Scale h' c cont
setHorizon h' (Observe h cont) = Observe h' cont
setHorizon h' (GetTime h cont) = GetTime h' cont
setHorizon h' (Choose h cont) = Choose h' cont

zcb :: Double -> Time -> Time -> Contract ()
zcb amt t1 t2 = do
  scale amt
  receiveOne

or :: Bool -> Contract a -> Contract a -> Contract a
or decision c1 c2 = if decision then c1 else c2

and :: Contract a -> Contract b -> Contract ()
and c1 c2 = do {c1 ; c2 ; return ()}

timebound :: Time -> Time -> Contract a -> Contract a
timebound t1 t2 c = do
  time <- getTime
  if t1 <= time && time <= t2 then
    c
  else
    timebound t1 t2 c

data Balances = Balances { _party :: Double, _counterparty :: Double } deriving Show
makeLenses ''Balances

defaultBalances = Balances 0.0 0.0
simpleContract = sendOne
executionResult = runState (interpret simpleContract) defaultBalances

interpret :: Contract a -> State Balances a
interpret (Pure a) = pure a
interpret (SendOne h cont) = do
  (%=) party (\x -> x - 1.0)
  (%=) counterparty ((+) 1.0)
  interpret cont
interpret (ReceiveOne h cont) = do
  (%=) party (1.0 +)
  (%=) counterparty (\x -> x - 1.0)
  interpret cont
interpret (Scale h c cont) = interpret cont
interpret (Observe h cont) = interpret (cont 2.0)
interpret (GetTime h cont) = interpret (cont 1)
interpret (Choose h cont) = interpret (cont True)