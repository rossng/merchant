{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

module Procedural where

import Control.Monad.State
import Control.Monad.Free
import Control.Lens

data ContractF next
  = SendOne Horizon next
  | ReceiveOne Horizon next
--  | Scale Double a
--  | Observe (Double -> a)
--  | GetTime (Time -> a)
  | GetChoice (Bool -> next)
  | If Bool next next
--  | After Time a
--  | And a a
--  | Fail
  deriving (Functor)

type Contract = Free ContractF

type Amount = Double
type Time = Int
type Scale = Double
data Horizon = Time Time | Infinite deriving (Eq, Ord)

sendOne :: Contract ()
sendOne = liftF (SendOne Infinite ())

receiveOne :: Contract ()
receiveOne = liftF (ReceiveOne Infinite ())

--scale :: Double -> Contract ()
--scale c = liftF (Scale c ())
--
--observe :: Contract Double
--observe = liftF (Observe id)
--
--getTime :: Contract Time
--getTime = liftF (GetTime id)
--
getChoice :: Contract Bool
getChoice = liftF (GetChoice id)

if' :: Bool -> Contract () -> Contract () -> Contract ()
if' b c1 c2 = Free (If b c1 c2)

--
--after :: Time -> Contract ()
--after t = liftF (After t ())

choose :: Contract () -> Contract () -> Contract ()
choose c1 c2 = do
  choice <- getChoice
  if' choice c1 c2

getHorizon :: Contract a -> Horizon
getHorizon (Pure a) = Infinite
getHorizon (Free (SendOne h cont)) = h
getHorizon (Free (ReceiveOne h cont)) = h
getHorizon c = Infinite
--getHorizon (Free (Scale c cont)) = Infinite
--getHorizon (Free (Observe cont)) = Infinite
--getHorizon (Free (GetTime cont)) = Infinite
--getHorizon (Free (Choose cont)) = Infinite
--getHorizon (Free (After t cont)) = Infinite

setHorizon :: Horizon -> Contract a -> Contract a
setHorizon h' (Free (SendOne h cont)) = Free $ SendOne h' cont
setHorizon h' (Free (ReceiveOne h cont)) = Free $ ReceiveOne h' cont
setHorizon h' c = c

zcb :: Double -> Time -> Time -> Contract ()
zcb amt t1 t2 = do
--  scale amt
--  after t1
  setHorizon (Time t2) sendOne

data Balances = Balances { _party :: Double, _counterparty :: Double } deriving Show
makeLenses ''Balances

defaultBalances = Balances 0.0 0.0
simpleContract = do
  choose receiveOne sendOne

executionResult = runStateT (interpret (1.0, 0) simpleContract) defaultBalances

interpret :: (Scale, Time) -> Contract a -> StateT Balances IO (Maybe a)
interpret (s,t) (Pure a) = pure (Just a)
interpret (s,t) (Free (SendOne h cont)) = do
  liftIO (putStrLn "Sending one")
  (%=) party (\x -> x - s)
  (%=) counterparty (s +)
  interpret (s,t) cont
interpret (s,t) (Free (ReceiveOne h cont)) = do
  liftIO (putStrLn "Receiving one")
  (%=) counterparty (\x -> x - s)
  (%=) party (s +)
  interpret (s,t) cont
interpret (s,t) (Free (GetChoice cont)) = do
  liftIO (putStr "Choose [True/False]: ")
  input <- liftIO getLine
  let choice = read input
  interpret (s,t) (cont choice)
interpret (s,t) (Free (If b c1 c2)) = if b then interpret (s,t) c1 else interpret (s,t) c2
--interpret (s,t) (Free (And c1 c2)) = do
--  interpret (s,t) c1
--  interpret (s,t) c2
--interpret (s,t) (Free (Or c1 c2)) = do
--  choice <- choose
--  if choice then interpret (s,t) c1
--  else interpret (s,t) c2
--interpret (s,t) (Scale c cont) = interpret (s*c,t) cont
--interpret (s,t) (Observe cont) = interpret (s,t) (cont 2.0)
--interpret (s,t) (GetTime cont) = interpret (s,t) (cont 1)
--interpret (s,t) (Choose cont) = interpret (s,t) (cont True)
--interpret (s,t) (After t' cont) = if t >= t' then interpret (s,t) cont else interpret (s,t) Fail
--interpret (s,t) (Free Fail) = pure Nothing