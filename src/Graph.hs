{-# LANGUAGE OverloadedStrings, TypeOperators #-}
module Graph where

import Language
import ALaCarte
import Render

import qualified Data.GraphViz.Attributes as A
import Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Commands

import Control.Monad.State
import qualified Data.Text.Lazy as T

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
  graphAlg (Give g) = do
    n <- increment
    g
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
  graphAlg (Scale f g) = do
    n <- increment
    g
    lift $ node n [A.textLabel (T.pack ("Scale " ++ printObservable f))]
    lift $ n --> (n + 1)

instance GraphAlg OriginalF where
  graphAlg (Truncate t g) = do
    n <- increment
    g
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
  graphAlg (Get g) = do
    n <- increment
    g
    lift $ node n [A.textLabel "Get"]
    lift $ n --> (n + 1)
  graphAlg (Anytime g) = do
    n <- increment
    g
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
  graphAlg (When o g) = do
    n <- increment
    g
    lift $ node n [A.textLabel (T.pack ("When " ++ printObservable o))]
    lift $ n --> (n + 1)
  graphAlg (AnytimeO o g) = do
    n <- increment
    g
    lift $ node n [A.textLabel (T.pack ("Anytime " ++ printObservable o))]
    lift $ n --> (n + 1)
  graphAlg (Until o g) = do
    n <- increment
    g
    lift $ node n [A.textLabel (T.pack ("Until " ++ printObservable o))]
    lift $ n --> (n + 1)

-- TODO: add the stored integer to the State
instance GraphAlg MonadicF where
  graphAlg (GetInt c) = c 0
  graphAlg (SetInt _ c) = c

instance (GraphAlg f, GraphAlg g) => GraphAlg (f :+ g) where
  graphAlg (L x) = graphAlg x
  graphAlg (R y) = graphAlg y
