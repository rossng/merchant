{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}
module Declarative where

import Control.Monad.State
import Control.Monad.Free
import Control.Lens
import Data.Text.Lazy

import qualified Data.GraphViz.Attributes as A
import Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic


data ContractF next
  = One Horizon
  | Scale Int next
  | ScaleObs next
  | Timebound (Time, Time) next
  | Give next
  | Or next next
  | And next next
  deriving (Functor)

type Contract = Free ContractF

type Time = Int
type Scale = Double
data Horizon = Time Time | Infinite deriving (Eq, Ord)

one' :: Contract a
one' = liftF (One Infinite)

scale' :: Int -> Contract a -> Contract a
scale' n c = Free (Scale n c)

scaleObs' :: Contract a -> Contract a
scaleObs' c = Free (ScaleObs c)

timebound' :: (Time, Time) -> Contract a -> Contract a
timebound' t c = Free (Timebound t c)

give' :: Contract a -> Contract a
give' c = Free (Give c)

or' :: Contract () -> Contract () -> Contract ()
or' c1 c2 = Free (Or c1 c2)

and' :: Contract () -> Contract () -> Contract ()
and' c1 c2 = Free (And c1 c2)

getHorizon :: Contract a -> Horizon
getHorizon (Pure a) = Infinite
getHorizon (Free (One h)) = h
getHorizon c = Infinite

setHorizon :: Horizon -> Contract a -> Contract a
setHorizon h' (Free (One h)) = Free $ One h'
setHorizon h' c = c

zcb :: Contract ()
zcb = and' (give' (scale' 10 one')) (timebound' (0,1) (scale' 11 one'))

interpret :: Contract a -> String
interpret (Pure a) = ""
interpret (Free (One h)) = "One"
interpret (Free (Scale n cont)) = "Scale(" ++ show n ++ "," ++ interpret cont ++ ")"
interpret (Free (ScaleObs cont)) = "ScaleObs(" ++ interpret cont ++ ")"
interpret (Free (Timebound (t1,t2) cont)) = "Timebound(" ++ show t1 ++ "," ++ show t2 ++ "," ++ interpret cont ++ ")"
interpret (Free (Give cont)) = "Give(" ++ interpret cont ++ ")"
interpret (Free (Or c1 c2)) = "Or(" ++ interpret c1 ++ "," ++ interpret c2 ++ ")"
interpret (Free (And c1 c2)) = "And(" ++ interpret c1 ++ "," ++ interpret c2 ++ ")"

toGraph :: Contract () -> G.DotGraph [Int]
toGraph contract = digraph (Str "contract") (toDot [] contract)

toDot :: [Int] -> Contract () -> Dot [Int]
toDot prefix (Pure a) = node prefix [A.textLabel "()"]
toDot prefix (Free (One h)) = node prefix [A.textLabel "One"]
toDot prefix (Free (Scale n cont)) = do
  node prefix [A.textLabel (pack ("Scale " ++ show n))]
  toDot (0 : prefix) cont
  prefix --> (0 : prefix)
toDot prefix (Free (ScaleObs cont)) = do
  node prefix [A.textLabel "ScaleObs"]
  toDot (0 : prefix) cont
  prefix --> (0 : prefix)
toDot prefix (Free (Timebound (t1,t2) cont)) = do
  node prefix [A.textLabel "Timebound"] -- todo add time bounds
  toDot (0 : prefix) cont
  prefix --> (0 : prefix)
toDot prefix (Free (Give cont)) = do
  node prefix [A.textLabel "Give"]
  toDot (0 : prefix) cont
  prefix --> (0 : prefix)
toDot prefix (Free (Or c1 c2)) = do
  node prefix [A.textLabel "Or"]
  toDot (0 : prefix) c1
  toDot (1 : prefix) c2
  prefix --> (0 : prefix)
  prefix --> (1 : prefix)
toDot prefix (Free (And c1 c2)) = do
  node prefix [A.textLabel "And"]
  toDot (0 : prefix) c1
  toDot (1 : prefix) c2
  prefix --> (0 : prefix)
  prefix --> (1 : prefix)

--data Balances = Balances { _party :: Double, _counterparty :: Double } deriving Show
--makeLenses ''Balances
--
--defaultBalances = Balances 0.0 0.0
--simpleContract = choose receiveOne' sendOne' `then'`
--  (do factor <- observe'
--      scale' factor
--      choose receiveOne' sendOne')
--
--executionResult = runStateT (interpret (1.0, 0) simpleContract) defaultBalances
