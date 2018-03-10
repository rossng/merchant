{-# LANGUAGE TypeOperators #-}
module Render where

import Control.Monad.Free

import Declarative
import ALaCarte

---- naive interpreter to text representation of contract
--interpret :: Contract -> String
--interpret (Pure a) = ""
--interpret (Free Zero) = "Zero"
--interpret (Free (One k)) = "One(" ++ show k ++ ")"
--interpret (Free (Give c)) = "Give(" ++ interpret c ++ ")"
--interpret (Free (And c1 c2)) = "And(" ++ interpret c1 ++ "," ++ interpret c2 ++ ")"
--interpret (Free (Or c1 c2)) = "Or(" ++ interpret c1 ++ "," ++ interpret c2 ++ ")"
--interpret (Free (Truncate t c)) = "Truncate(" ++ show t ++ "," ++ interpret c ++ ")"
--interpret (Free (Scale obs c)) = "ScaleObs(" ++ interpret c ++ ")"
--interpret (Free (Get c)) = "Get(" ++ interpret c ++ ")"
--interpret (Free (Anytime c)) = "Anytime(" ++ interpret c ++ ")"


class Functor f => Render f where
  renderAlg :: f String -> String

instance Render ContractF where
  renderAlg Zero = "Zero"
  renderAlg (One k) = "One(" ++ show k ++ ")"
  renderAlg (Give c) = "Give(" ++ c ++ ")"
  renderAlg (And c1 c2) = "And(" ++ c1 ++ "," ++ c2 ++ ")"
  renderAlg (Or c1 c2) = "Or(" ++ c1 ++ "," ++ c2 ++ ")"
  renderAlg (Scale n c) = "Scale(" ++ show n ++ "," ++ c ++ ")"

instance Render OriginalF where
  renderAlg (Truncate t c) = "Truncate(" ++ show t ++ "," ++ c ++ ")"
  renderAlg (Then c1 c2) = "Then(" ++ c1 ++ "," ++ c2 ++ ")"
  renderAlg (Get c) = "Get(" ++ c ++ ")"
  renderAlg (Anytime c) = "Anytime(" ++ c ++ ")"

instance Render ExtendedF where
  renderAlg (Cond o c1 c2) = "Cond(" ++ show o ++ "," ++ c1 ++ "," ++ c2 ++ ")"
  renderAlg (When o c) = "When(" ++ show o ++ "," ++ c ++ ")"
  renderAlg (AnytimeO o c) = "AnytimeO(" ++ show o ++ "," ++ c ++ ")"
  renderAlg (Until o c) = "Until(" ++ show o ++ "," ++ c ++ ")"

instance (Render f, Render g) => Render (f :+ g) where
  renderAlg (L x) = renderAlg x
  renderAlg (R y) = renderAlg y

prettyPrint :: Contract -> String
prettyPrint (Pure _) = ""
prettyPrint c = handle (const "") renderAlg c