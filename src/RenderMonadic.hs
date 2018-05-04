{-# LANGUAGE TypeOperators, GADTs #-}
module RenderMonadic where

import Control.Monad.Free
import Control.Monad.State

import Declarative
import Observable
import ALaCarte

class Functor f => RenderM f where
  renderMAlg :: f (State Int String) -> (State Int String)

instance RenderM ContractF where
  renderMAlg Zero = return "Zero"
  renderMAlg (One k) = return $ "One(" ++ show k ++ ")"
  renderMAlg (Give c) = do
    c' <- c
    return $ "Give(" ++ c' ++ ")"
  renderMAlg (And c1 c2) = do
    c1' <- c1
    c2' <- c2
    return $ "And(" ++ c1' ++ "," ++ c2' ++ ")"
  renderMAlg (Or c1 c2) = do
    c1' <- c1
    c2' <- c2
    return $ "Or(" ++ c1' ++ "," ++ c2' ++ ")"
  renderMAlg (Scale n c) = do
    c' <- c
    return $ "Scale(" ++ printObservable n ++ "," ++ c' ++ ")"

instance RenderM OriginalF where
  renderMAlg (Truncate t c) = do
    c' <- c
    return $ "Truncate(" ++ show t ++ "," ++ c' ++ ")"
  renderMAlg (Then c1 c2) = do
    c1' <- c1
    c2' <- c2
    return $ "Then(" ++ c1' ++ "," ++ c2' ++ ")"
  renderMAlg (Get c) = do
    c' <- c
    return $ "Get(" ++ c' ++ ")"
  renderMAlg (Anytime c) = do
    c' <- c
    return $ "Anytime(" ++ c' ++ ")"

instance RenderM ExtendedF where
  renderMAlg (Cond o c1 c2) = do
    c1' <- c1
    c2' <- c2
    return $ "Cond(" ++ printObservable o ++ "," ++ c1' ++ "," ++ c2' ++ ")"
  renderMAlg (When o c) = do
    c' <- c
    return $ "When(" ++ printObservable o ++ "," ++ c' ++ ")"
  renderMAlg (AnytimeO o c) = do
    c' <- c
    return $ "AnytimeO(" ++ printObservable o ++ "," ++ c' ++ ")"
  renderMAlg (Until o c) = do
    c' <- c
    return $ "Until(" ++ printObservable o ++ "," ++ c' ++ ")"

instance RenderM MonadicF where
  renderMAlg (GetInt c) = do
    i <- get
    c i
  renderMAlg (SetInt i c) = do
    put i
    c

instance (RenderM f, RenderM g) => RenderM (f :+ g) where
  renderMAlg (L x) = renderMAlg x
  renderMAlg (R y) = renderMAlg y

printContractM :: ContractM -> String
printContractM (Pure _) = ""
printContractM c = evalState (handle (const (return "")) renderMAlg c) 0

printObservable :: Show a => Obs a -> String
printObservable (External addr) = "[External " ++ addr ++ "]"
printObservable (Constant c) = "[Constant " ++ show c ++ "]"
printObservable (After t) = "[After " ++ show t ++ "]"
printObservable (OAnd o1 o2) = "[" ++ printObservable o1 ++ " && " ++ printObservable o2 ++ "]"
printObservable (OGreaterThan o1 o2) = "[" ++ printObservable o1 ++ " > " ++ printObservable o2 ++ "]"
printObservable (OSubtract o1 o2) = "[" ++ printObservable o1 ++ " - " ++ printObservable o2 ++ "]"
