{-# LANGUAGE DeriveFunctor, OverloadedStrings, TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverlappingInstances, GADTs #-}
module Observable where

import Control.Monad.Free

import ALaCarte

type Time = Int

data Obs a where
  External :: String -> Obs a
  Constant :: a -> Obs a
  After :: Time -> Obs Bool
  OAnd :: Obs Bool -> Obs Bool -> Obs Bool