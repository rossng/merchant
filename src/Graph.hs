{-# LANGUAGE OverloadedStrings #-}

module Graph where

import           Data.Graph.Inductive
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Generalised   as G
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                    as L
import           Data.Word

ex2 :: G.DotGraph String
ex2 = digraph (Str "ex2") $ do
    graphAttrs [RankDir FromLeft]
    nodeAttrs  [style filled]

--     outside the box(es)
    node "request"                 [ textLabel "REQUEST"
                                   , shape     BoxShape]
    node "response"                [ textLabel "RESPONSE"
                                   , shape     BoxShape]

    "request" --> "response"