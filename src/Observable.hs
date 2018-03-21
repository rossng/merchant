{-# LANGUAGE DeriveFunctor, OverloadedStrings, TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverlappingInstances, GADTs #-}
module Observable where

import Control.Monad.Free

import ALaCarte

type Time = Int

data Obs a where
  External :: String -> Obs Int
  Constant :: a -> Obs a
  After :: Time -> Obs Bool
  OAnd :: Obs Bool -> Obs Bool -> Obs Bool