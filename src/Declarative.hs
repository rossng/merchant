{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}
module Declarative where

import Control.Monad.State
import Control.Monad.Free
import Control.Monad.State
import Control.Lens
import Data.Text.Lazy

import qualified Data.GraphViz.Attributes as A
import Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Commands


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

-- naive interpreter to text representation of contract
interpret :: Contract a -> String
interpret (Pure a) = ""
interpret (Free (One h)) = "One"
interpret (Free (Scale n cont)) = "Scale(" ++ show n ++ "," ++ interpret cont ++ ")"
interpret (Free (ScaleObs cont)) = "ScaleObs(" ++ interpret cont ++ ")"
interpret (Free (Timebound (t1,t2) cont)) = "Timebound(" ++ show t1 ++ "," ++ show t2 ++ "," ++ interpret cont ++ ")"
interpret (Free (Give cont)) = "Give(" ++ interpret cont ++ ")"
interpret (Free (Or c1 c2)) = "Or(" ++ interpret c1 ++ "," ++ interpret c2 ++ ")"
interpret (Free (And c1 c2)) = "And(" ++ interpret c1 ++ "," ++ interpret c2 ++ ")"

handle :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
handle gen alg (Pure x) = gen x
handle gen alg (Free x) = alg (fmap (handle gen alg) x)

-- algebraic algebra to text representation of contract
textAlg :: ContractF String -> String
textAlg (One h) = "One"
textAlg (Scale n cont) = "Scale(" ++ show n ++ "," ++ cont ++ ")"
textAlg (ScaleObs cont) = "ScaleObs(" ++ cont ++ ")"
textAlg (Timebound (t1,t2) cont) = "Timebound(" ++ show t1 ++ "," ++ show t2 ++ "," ++ cont ++ ")"
textAlg (Give cont) = "Give(" ++ cont ++ ")"
textAlg (Or c1 c2) = "Or(" ++ c1 ++ "," ++ c2 ++ ")"
textAlg (And c1 c2) = "And(" ++ c1 ++ "," ++ c2 ++ ")"

prettyPrint :: Contract () -> String
prettyPrint (Pure _) = ""
prettyPrint c = handle (const "") textAlg c

renderToGraph :: Contract () -> FilePath -> IO FilePath
renderToGraph c fp = runGraphviz (toGraph c) Png fp

toGraph :: Contract () -> G.DotGraph [Int]
toGraph contract = digraph (Str "contract") (toDot [] contract)

-- very naive interpreter for contracts to graphs using prefix list
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
  node prefix [A.textLabel (pack ("Timebound " ++ show t1 ++ " " ++ show t2))]
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

renderToGraph' :: Contract () -> FilePath -> IO FilePath
renderToGraph' c fp = runGraphviz (toGraph' c) Png fp

toGraph' :: Contract () -> G.DotGraph Int
toGraph' contract = digraph (Str "contract") (evalStateT (toDot' contract) 0)

increment :: StateT Int (DotM Int) Int
increment = do
  identifier <- get
  put (identifier + 1)
  return identifier

-- less naive interpreter for contracts to graphs using state
toDot' :: Contract () -> StateT Int (DotM Int) ()
toDot' (Pure a) = do
  n <- increment
  lift $ node n [A.textLabel "()"]
toDot' (Free (One h)) = do
  n <- increment
  lift $ node n [A.textLabel "One"]
toDot' (Free (Scale f cont)) = do
  n <- increment
  lift $ node n [A.textLabel (pack ("Scale " ++ show f))]
  lift $ n --> (n + 1)
  toDot' cont
toDot' (Free (ScaleObs cont)) = do
  n <- increment
  lift $ node n [A.textLabel "ScaleObs"]
  lift $ n --> (n + 1)
  toDot' cont
toDot' (Free (Timebound (t1,t2) cont)) = do
  n <- increment
  lift $ node n [A.textLabel (pack ("Timebound " ++ show t1 ++ " " ++ show t2))]
  lift $ n --> (n + 1)
  toDot' cont
toDot' (Free (Give cont)) = do
  n <- increment
  lift $ node n [A.textLabel "Give"]
  lift $ n --> (n + 1)
  toDot' cont
toDot' (Free (Or c1 c2)) = do
  n <- increment
  lift $ node n [A.textLabel "Or"]
  lift $ n --> (n + 1)
  toDot' c1
  branch <- get
  lift $ n --> branch
  toDot' c2
toDot' (Free (And c1 c2)) = do
  n <- increment
  lift $ node n [A.textLabel "And"]
  lift $ n --> (n + 1)
  toDot' c1
  branch <- get
  lift $ n --> branch
  toDot' c2

renderToGraphAlg :: Contract () -> FilePath -> IO FilePath
renderToGraphAlg c fp = runGraphviz (toGraphAlg c) Png fp

toGraphAlg :: Contract () -> G.DotGraph Int
toGraphAlg contract = digraph (Str "contract") (evalStateT finalState 0)
  where finalState = handle pure dotAlg contract

dotAlg :: ContractF (StateT Int (DotM Int) ()) -> StateT Int (DotM Int) ()
dotAlg (One h) = do
  n <- increment -- think about whether we actually want to increment at the leaves
  lift $ node n [A.textLabel "One"]
dotAlg (Scale f graph) = do
  n <- increment
  graph
  lift $ node n [A.textLabel (pack ("Scale" ++ show f))]
  lift $ n --> (n + 1)
dotAlg (ScaleObs graph) = do
  n <- increment
  graph
  lift $ node n [A.textLabel "ScaleObs"]
  lift $ n --> (n + 1)
dotAlg (Timebound (t1,t2) graph) = do
  n <- increment
  graph
  lift $ node n [A.textLabel (pack ("Timebound " ++ show t1 ++ " " ++ show t2))]
  lift $ n --> (n + 1)
dotAlg (Give graph) = do
  n <- increment
  graph
  lift $ node n [A.textLabel "Give"]
  lift $ n --> (n + 1)
dotAlg (Or graph1 graph2) = do
  n <- increment
  graph1
  m <- get
  graph2
  lift $ node n [A.textLabel "Or"]
  lift $ n --> (n + 1)
  lift $ n --> m
dotAlg (And graph1 graph2) = do
  n <- increment
  graph1
  m <- get
  graph2
  lift $ node n [A.textLabel "And"]
  lift $ n --> (n + 1)
  lift $ n --> m

--data Balances = Balances { _party :: Double, _counterparty :: Double } deriving Show
--makeLenses ''Balances
--
--defaultBalances = Balances 0.0 0.0
--simpleContract = choose receiveOne' sendOne' `then'`
--  (do factor <- observe'
--      scale' factor
--      choose receiveOne' sendOne')

--executionResult = runStateT (interpret (1.0, 0) simpleContract) defaultBalances
