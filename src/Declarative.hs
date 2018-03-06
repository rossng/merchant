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
  = Zero
  | One
  | Give next
  | And next next
  | Or next next
  | Truncate Time next
  | Then next next
  | Scale Observable next
  | Get next
  | Anytime next
  deriving (Functor)

type Contract = Free ContractF

type Time = Int
type Scale = Double
data Horizon = Time Time | Infinite deriving (Eq, Ord, Show)
data Observable = External String | Constant Int deriving (Show)

zero' :: Contract a
zero' = liftF Zero

one' :: Contract a
one' = liftF One

give' :: Contract a -> Contract a
give' c = Free (Give c)

and' :: Contract () -> Contract () -> Contract ()
and' c1 c2 = Free (And c1 c2)

or' :: Contract () -> Contract () -> Contract ()
or' c1 c2 = Free (Or c1 c2)

truncate' :: Time -> Contract a -> Contract a
truncate' t c = Free (Truncate t c)

then' :: Contract() -> Contract () -> Contract ()
then' c1 c2 = Free (Then c1 c2)

scale' :: Observable -> Contract a -> Contract a
scale' n c = Free (Scale n c)

scaleK' :: Int -> Contract a -> Contract a
scaleK' n c = Free (Scale (Constant n) c)

get' :: Contract a -> Contract a
get' c = Free (Get c)

anytime' :: Contract a -> Contract a
anytime' c = Free (Anytime c)

zcb :: Time -> Int -> Contract ()
zcb t x = scaleK' x (get' (truncate' t (one')))

zcbExample :: Contract ()
zcbExample = zcb 1 10

-- naive interpreter to text representation of contract
interpret :: Contract a -> String
interpret (Pure a) = ""
interpret (Free Zero) = "Zero"
interpret (Free One) = "One"
interpret (Free (Give c)) = "Give(" ++ interpret c ++ ")"
interpret (Free (And c1 c2)) = "And(" ++ interpret c1 ++ "," ++ interpret c2 ++ ")"
interpret (Free (Or c1 c2)) = "Or(" ++ interpret c1 ++ "," ++ interpret c2 ++ ")"
interpret (Free (Truncate t c)) = "Truncate(" ++ show t ++ "," ++ interpret c ++ ")"
interpret (Free (Scale obs c)) = "ScaleObs(" ++ interpret c ++ ")"
interpret (Free (Get c)) = "Get(" ++ interpret c ++ ")"
interpret (Free (Anytime c)) = "Anytime(" ++ interpret c ++ ")"

handle :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
handle gen alg (Pure x) = gen x
handle gen alg (Free x) = alg (fmap (handle gen alg) x)

-- algebraic algebra to text representation of contract
textAlg :: ContractF String -> String
textAlg Zero = "Zero"
textAlg One = "One"
textAlg (Give c) = "Give(" ++ c ++ ")"
textAlg (And c1 c2) = "And(" ++ c1 ++ "," ++ c2 ++ ")"
textAlg (Or c1 c2) = "Or(" ++ c1 ++ "," ++ c2 ++ ")"
textAlg (Truncate t c) = "Truncate(" ++ show t ++ "," ++ c ++ ")"
textAlg (Then c1 c2) = "Then(" ++ c1 ++ "," ++ c2 ++ ")"
textAlg (Scale n c) = "Scale(" ++ show n ++ "," ++ c ++ ")"
textAlg (Get c) = "Get(" ++ c ++ ")"
textAlg (Anytime c) = "Anytime(" ++ c ++ ")"

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
toDot prefix (Free Zero) = node prefix [A.textLabel "Zero"]
toDot prefix (Free One) = node prefix [A.textLabel "One"]
toDot prefix (Free (Give c)) = do
  node prefix [A.textLabel "Give"]
  toDot (0 : prefix) c
  prefix --> (0 : prefix)
toDot prefix (Free (And c1 c2)) = do
  node prefix [A.textLabel "And"]
  toDot (0 : prefix) c1
  toDot (1 : prefix) c2
  prefix --> (0 : prefix)
  prefix --> (1 : prefix)
toDot prefix (Free (Or c1 c2)) = do
  node prefix [A.textLabel "Or"]
  toDot (0 : prefix) c1
  toDot (1 : prefix) c2
  prefix --> (0 : prefix)
  prefix --> (1 : prefix)
toDot prefix (Free (Truncate t c)) = do
  node prefix [A.textLabel (pack ("Timebound " ++ show t))]
  toDot (0 : prefix) c
  prefix --> (0 : prefix)
toDot prefix (Free (Then c1 c2)) = do
  node prefix [A.textLabel "Then"]
  toDot (0 : prefix) c1
  toDot (1 : prefix) c2
  prefix --> (0 : prefix)
  prefix --> (1 : prefix)
toDot prefix (Free (Scale n c)) = do
  node prefix [A.textLabel (pack ("Scale " ++ show n))]
  toDot (0 : prefix) c
  prefix --> (0 : prefix)
toDot prefix (Free (Get c)) = do
  node prefix [A.textLabel "Get"]
  toDot (0 : prefix) c
  prefix --> (0 : prefix)
toDot prefix (Free (Anytime c)) = do
  node prefix [A.textLabel "Anytime"]
  toDot (0 : prefix) c
  prefix --> (0 : prefix)

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
toDot' (Free Zero) = do
  n <- increment
  lift $ node n [A.textLabel "Zero"]
toDot' (Free One) = do
  n <- increment
  lift $ node n [A.textLabel "One"]
toDot' (Free (Give cont)) = do
  n <- increment
  lift $ node n [A.textLabel "Give"]
  lift $ n --> (n + 1)
  toDot' cont
toDot' (Free (And c1 c2)) = do
  n <- increment
  lift $ node n [A.textLabel "And"]
  lift $ n --> (n + 1)
  toDot' c1
  branch <- get
  lift $ n --> branch
  toDot' c2
toDot' (Free (Or c1 c2)) = do
  n <- increment
  lift $ node n [A.textLabel "Or"]
  lift $ n --> (n + 1)
  toDot' c1
  branch <- get
  lift $ n --> branch
  toDot' c2
toDot' (Free (Truncate t c)) = do
  n <- increment
  lift $ node n [A.textLabel (pack ("Truncate " ++ show t))]
  lift $ n --> (n + 1)
  toDot' c
toDot' (Free (Then c1 c2)) = do
  n <- increment
  lift $ node n [A.textLabel "Then"]
  lift $ n --> (n + 1)
  toDot' c1
  branch <- get
  lift $ n --> branch
  toDot' c2
toDot' (Free (Scale f c)) = do
  n <- increment
  lift $ node n [A.textLabel (pack ("Scale " ++ show f))]
  lift $ n --> (n + 1)
  toDot' c
toDot' (Free (Get c)) = do
  n <- increment
  lift $ node n [A.textLabel "Get"]
  lift $ n --> (n + 1)
  toDot' c
toDot' (Free (Anytime c)) = do
  n <- increment
  lift $ node n [A.textLabel "Anytime"]
  lift $ n --> (n + 1)
  toDot' c

renderToGraphAlg :: Contract () -> FilePath -> IO FilePath
renderToGraphAlg c = runGraphviz (toGraphAlg c) Png

-- proper interpreter for contract -> graph rendering
toGraphAlg :: Contract () -> G.DotGraph Int
toGraphAlg contract = digraph (Str "contract") (evalStateT finalState 0)
  where finalState = handle pure dotAlg contract

-- proper handler for contract -> graph rendering
dotAlg :: ContractF (StateT Int (DotM Int) ()) -> StateT Int (DotM Int) ()
dotAlg Zero = do
  n <- increment -- think about whether we actually want to increment at the leaves
  lift $ node n [A.textLabel "Zero"]
dotAlg One = do
  n <- increment -- think about whether we actually want to increment at the leaves
  lift $ node n [A.textLabel "One"]
dotAlg (Give graph) = do
  n <- increment
  graph
  lift $ node n [A.textLabel "Give"]
  lift $ n --> (n + 1)
dotAlg (And graph1 graph2) = do
  n <- increment
  graph1
  m <- get
  graph2
  lift $ node n [A.textLabel "And"]
  lift $ n --> (n + 1)
  lift $ n --> m
dotAlg (Or graph1 graph2) = do
  n <- increment
  graph1
  m <- get
  graph2
  lift $ node n [A.textLabel "Or"]
  lift $ n --> (n + 1)
  lift $ n --> m
dotAlg (Truncate t graph) = do
  n <- increment
  graph
  lift $ node n [A.textLabel (pack ("Truncate " ++ show t))]
  lift $ n --> (n + 1)
dotAlg (Then graph1 graph2) = do
  n <- increment
  graph1
  m <- get
  graph2
  lift $ node n [A.textLabel "Then"]
  lift $ n --> (n + 1)
  lift $ n --> m
dotAlg (Scale f graph) = do
  n <- increment
  graph
  lift $ node n [A.textLabel (pack ("Scale" ++ show f))]
  lift $ n --> (n + 1)
dotAlg (Get graph) = do
  n <- increment
  graph
  lift $ node n [A.textLabel "Get"]
  lift $ n --> (n + 1)
dotAlg (Anytime graph) = do
  n <- increment
  graph
  lift $ node n [A.textLabel "Anytime"]
  lift $ n --> (n + 1)

horizon :: Contract () -> Horizon
horizon c = handle (const Infinite) horizonAlg c

horizonAlg :: ContractF Horizon -> Horizon
horizonAlg Zero = Infinite
horizonAlg One = Infinite
horizonAlg (Give c) = c
horizonAlg (And c1 c2) = max c1 c2
horizonAlg (Or c1 c2) = max c1 c2
horizonAlg (Truncate t c) = min (Time t) c
horizonAlg (Then c1 c2) = max c1 c2
horizonAlg (Scale obs c) = c
horizonAlg (Get c) = c
horizonAlg (Anytime c) = c