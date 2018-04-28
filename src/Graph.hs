{-# LANGUAGE OverloadedStrings, TypeOperators #-}
module Graph where

import Declarative
import ALaCarte
import Observable
import Render

import qualified Data.GraphViz.Attributes as A
import Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Commands

import Control.Monad.State
import qualified Data.Text.Lazy as T

renderToNaiveGraphAlg :: Contract -> FilePath -> IO FilePath
renderToNaiveGraphAlg c = runGraphviz (toNaiveGraphAlg c) Png

toNaiveGraphAlg :: Contract -> G.DotGraph String
toNaiveGraphAlg contract = digraph (Str "contract") (handle pure naiveGraphAlg contract)

class Functor f => NaiveGraphAlg f where
  naiveGraphAlg :: f (DotM String ()) -> DotM String ()

instance NaiveGraphAlg ContractF where
  naiveGraphAlg Zero = do
    node "Zero" [A.textLabel "Zero"]
  naiveGraphAlg (One k) = do
    node "One" [A.textLabel (T.pack $ "One(" ++ show k ++ ")")]
  naiveGraphAlg (Give c) = do
    node "Give" [A.textLabel "Give"]
    c
  naiveGraphAlg (And c1 c2) = do
    node "And" [A.textLabel "And"]
    c1
    c2
  naiveGraphAlg (Scale o c) = do
    node "Scale" [A.textLabel "Scale"]
    c

instance NaiveGraphAlg OriginalF where
  naiveGraphAlg = undefined

instance NaiveGraphAlg ExtendedF where
  naiveGraphAlg = undefined

instance (NaiveGraphAlg f, NaiveGraphAlg g) => NaiveGraphAlg (f :+ g) where
  naiveGraphAlg (L x) = naiveGraphAlg x
  naiveGraphAlg (R y) = naiveGraphAlg y

renderToGraphAlg :: Contract -> FilePath -> IO FilePath
renderToGraphAlg c = runGraphviz (toGraphAlg c) Png

-- proper interpreter for contract -> graph rendering
toGraphAlg :: Contract -> G.DotGraph Int
toGraphAlg contract = digraph (Str "contract") (evalStateT finalState 0)
  where finalState = handle pure graphAlg contract

class Functor f => GraphAlg f where
  graphAlg :: f (StateT Int (DotM Int) ()) -> StateT Int (DotM Int) ()

increment :: StateT Int (DotM Int) Int
increment = do
  identifier <- get
  put (identifier + 1)
  return identifier

instance GraphAlg ContractF where
  graphAlg Zero = do
    n <- increment -- think about whether we actually want to increment at the leaves
    lift $ node n [A.textLabel "Zero"]
  graphAlg (One k) = do
    n <- increment -- think about whether we actually want to increment at the leaves
    lift $ node n [A.textLabel (T.pack ("One(" ++ show k ++ ")"))]
  graphAlg (Give graph) = do
    n <- increment
    graph
    lift $ node n [A.textLabel "Give"]
    lift $ n --> (n + 1)
  graphAlg (And graph1 graph2) = do
    n <- increment
    graph1
    m <- get
    graph2
    lift $ node n [A.textLabel "And"]
    lift $ n --> (n + 1)
    lift $ n --> m
  graphAlg (Or graph1 graph2) = do
    n <- increment
    graph1
    m <- get
    graph2
    lift $ node n [A.textLabel "Or"]
    lift $ n --> (n + 1)
    lift $ n --> m
  graphAlg (Scale f graph) = do
    n <- increment
    graph
    lift $ node n [A.textLabel (T.pack ("Scale " ++ printObservable f))]
    lift $ n --> (n + 1)

instance GraphAlg OriginalF where
  graphAlg (Truncate t graph) = do
    n <- increment
    graph
    lift $ node n [A.textLabel (T.pack ("Truncate " ++ show t))]
    lift $ n --> (n + 1)
  graphAlg (Then graph1 graph2) = do
    n <- increment
    graph1
    m <- get
    graph2
    lift $ node n [A.textLabel "Then"]
    lift $ n --> (n + 1)
    lift $ n --> m
  graphAlg (Get graph) = do
    n <- increment
    graph
    lift $ node n [A.textLabel "Get"]
    lift $ n --> (n + 1)
  graphAlg (Anytime graph) = do
    n <- increment
    graph
    lift $ node n [A.textLabel "Anytime"]
    lift $ n --> (n + 1)

instance GraphAlg ExtendedF where
  graphAlg (Cond o graph1 graph2) = do
    n <- increment
    graph1
    m <- get
    graph2
    lift $ node n [A.textLabel (T.pack ("Cond " ++ printObservable o))]
    lift $ n --> (n+1)
    lift $ n --> m
  graphAlg (When o graph) = do
    n <- increment
    graph
    lift $ node n [A.textLabel (T.pack ("When " ++ printObservable o))]
    lift $ n --> (n + 1)
  graphAlg (AnytimeO o graph) = do
    n <- increment
    graph
    lift $ node n [A.textLabel (T.pack ("Anytime " ++ printObservable o))]
    lift $ n --> (n + 1)
  graphAlg (Until o graph) = do
    n <- increment
    graph
    lift $ node n [A.textLabel (T.pack ("Until " ++ printObservable o))]
    lift $ n --> (n + 1)

instance GraphAlg MonadicF where
  graphAlg (GetInt c) = c 0
  graphAlg (SetInt i c) = c

instance (GraphAlg f, GraphAlg g) => GraphAlg (f :+ g) where
  graphAlg (L x) = graphAlg x
  graphAlg (R y) = graphAlg y

-- very naive interpreter for contracts to graphs using prefix list
--toGraph :: Contract -> G.DotGraph [Int]
--toGraph contract = digraph (Str "contract") (toDot [] contract)
--toDot :: [Int] -> Contract -> Dot [Int]
--toDot prefix (Pure a) = node prefix [A.textLabel "()"]
--toDot prefix (Free Zero) = node prefix [A.textLabel "Zero"]
--toDot prefix (Free (One k)) = node prefix [A.textLabel (T.pack ("One(" ++ show k ++ ")"))]
--toDot prefix (Free (Give c)) = do
--  node prefix [A.textLabel "Give"]
--  toDot (0 : prefix) c
--  prefix --> (0 : prefix)
--toDot prefix (Free (And c1 c2)) = do
--  node prefix [A.textLabel "And"]
--  toDot (0 : prefix) c1
--  toDot (1 : prefix) c2
--  prefix --> (0 : prefix)
--  prefix --> (1 : prefix)
--toDot prefix (Free (Or c1 c2)) = do
--  node prefix [A.textLabel "Or"]
--  toDot (0 : prefix) c1
--  toDot (1 : prefix) c2
--  prefix --> (0 : prefix)
--  prefix --> (1 : prefix)
--toDot prefix (Free (Truncate t c)) = do
--  node prefix [A.textLabel (T.pack ("Timebound " ++ show t))]
--  toDot (0 : prefix) c
--  prefix --> (0 : prefix)
--toDot prefix (Free (Then c1 c2)) = do
--  node prefix [A.textLabel "Then"]
--  toDot (0 : prefix) c1
--  toDot (1 : prefix) c2
--  prefix --> (0 : prefix)
--  prefix --> (1 : prefix)
--toDot prefix (Free (Scale n c)) = do
--  node prefix [A.textLabel (T.pack ("Scale " ++ show n))]
--  toDot (0 : prefix) c
--  prefix --> (0 : prefix)
--toDot prefix (Free (Get c)) = do
--  node prefix [A.textLabel "Get"]
--  toDot (0 : prefix) c
--  prefix --> (0 : prefix)
--toDot prefix (Free (Anytime c)) = do
--  node prefix [A.textLabel "Anytime"]
--  toDot (0 : prefix) c
--  prefix --> (0 : prefix)

--renderToGraph :: Contract -> FilePath -> IO FilePath
--renderToGraph c = runGraphviz (toGraph c) Png
--
--renderToGraph' :: Contract -> FilePath -> IO FilePath
--renderToGraph' c = runGraphviz (toGraph' c) Png
--
--toGraph' :: Contract -> G.DotGraph Int
--toGraph' contract = digraph (Str "contract") (evalStateT (toDot' contract) 0)
--
--
---- less naive interpreter for contracts to graphs using state
--toDot' :: Contract -> StateT Int (DotM Int) ()
--toDot' (Pure a) = do
--  n <- increment
--  lift $ node n [A.textLabel "()"]
--toDot' (Free Zero) = do
--  n <- increment
--  lift $ node n [A.textLabel "Zero"]
--toDot' (Free (One k)) = do
--  n <- increment
--  lift $ node n [A.textLabel (T.pack ("One" ++ show k ++ ")"))]
--toDot' (Free (Give cont)) = do
--  n <- increment
--  lift $ node n [A.textLabel "Give"]
--  lift $ n --> (n + 1)
--  toDot' cont
--toDot' (Free (And c1 c2)) = do
--  n <- increment
--  lift $ node n [A.textLabel "And"]
--  lift $ n --> (n + 1)
--  toDot' c1
--  branch <- get
--  lift $ n --> branch
--  toDot' c2
--toDot' (Free (Or c1 c2)) = do
--  n <- increment
--  lift $ node n [A.textLabel "Or"]
--  lift $ n --> (n + 1)
--  toDot' c1
--  branch <- get
--  lift $ n --> branch
--  toDot' c2
--toDot' (Free (Truncate t c)) = do
--  n <- increment
--  lift $ node n [A.textLabel (T.pack ("Truncate " ++ show t))]
--  lift $ n --> (n + 1)
--  toDot' c
--toDot' (Free (Then c1 c2)) = do
--  n <- increment
--  lift $ node n [A.textLabel "Then"]
--  lift $ n --> (n + 1)
--  toDot' c1
--  branch <- get
--  lift $ n --> branch
--  toDot' c2
--toDot' (Free (Scale f c)) = do
--  n <- increment
--  lift $ node n [A.textLabel (T.pack ("Scale " ++ show f))]
--  lift $ n --> (n + 1)
--  toDot' c
--toDot' (Free (Get c)) = do
--  n <- increment
--  lift $ node n [A.textLabel "Get"]
--  lift $ n --> (n + 1)
--  toDot' c
--toDot' (Free (Anytime c)) = do
--  n <- increment
--  lift $ node n [A.textLabel "Anytime"]
--  lift $ n --> (n + 1)
--  toDot' c