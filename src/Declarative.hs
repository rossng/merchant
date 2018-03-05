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

-- less naive interpreter for contracts to graphs using state
toDot' :: Contract () -> StateT Int (DotM Int) ()
toDot' (Pure a) = do
  identifier <- get
  lift $ node identifier [A.textLabel "()"]
  put (identifier + 1)
toDot' (Free (One h)) = do
  identifier <- get
  lift $ node identifier [A.textLabel "One"]
  put (identifier + 1)
toDot' (Free (Scale n cont)) = do
  identifier <- get
  lift $ node identifier [A.textLabel (pack ("Scale " ++ show n))]
  put (identifier + 1)
  lift $ identifier --> (identifier + 1)
  toDot' cont
toDot' (Free (ScaleObs cont)) = do
  identifier <- get
  lift $ node identifier [A.textLabel "ScaleObs"]
  put (identifier + 1)
  lift $ identifier --> (identifier + 1)
  toDot' cont
toDot' (Free (Timebound (t1,t2) cont)) = do
  identifier <- get
  lift $ node identifier [A.textLabel (pack ("Timebound " ++ show t1 ++ " " ++ show t2))]
  put (identifier + 1)
  lift $ identifier --> (identifier + 1)
  toDot' cont
toDot' (Free (Give cont)) = do
  identifier <- get
  lift $ node identifier [A.textLabel "Give"]
  put (identifier + 1)
  lift $ identifier --> (identifier + 1)
  toDot' cont
toDot' (Free (Or c1 c2)) = do
  identifier <- get
  lift $ node identifier [A.textLabel "Or"]
  put (identifier + 1)
  lift $ identifier --> (identifier + 1)
  toDot' c1
  branch <- get
  lift $ identifier --> branch
  toDot' c2
toDot' (Free (And c1 c2)) = do
  identifier <- get
  lift $ node identifier [A.textLabel "And"]
  put (identifier + 1)
  lift $ identifier --> (identifier + 1)
  toDot' c1
  branch <- get
  lift $ identifier --> branch
  toDot' c2

--dotAlg :: ContractF ([Int], Dot [Int]) -> ([Int], Dot [Int])
--dotAlg (One h) = ([], node [] [A.textLabel "One"])
--dotAlg (Scale n (prefix, graph)) = ((0:prefix), do
--  node (0:prefix) [A.textLabel (pack ("Scale" ++ show n))]
--  (0:prefix) --> prefix
--  graph)
--dotAlg (ScaleObs (prefix, graph)) = ((0:prefix), do
--  node (0:prefix) [A.textLabel "ScaleObs"]
--  (0:prefix) --> prefix
--  graph)
--dotAlg (Timebound (t1,t2) (prefix, graph)) = ((0:prefix), do
--  node (0:prefix) [A.textLabel (pack ("Timebound " ++ show t1 ++ " " ++ show t2))]
--  (0:prefix) --> prefix
--  graph)
--dotAlg (Give (prefix, graph)) = ((0:prefix), do
--  node (0:prefix) [A.textLabel "Give"]
--  (0:prefix) --> prefix
--  graph)
--dotAlg (Or (pfix1, grph1) (pfix2, grph2)) = ((0:prefix), do
--  node (0:prefix) [A.textLabel "Give"]
--  (0:prefix) --> prefix
--  graph)



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
