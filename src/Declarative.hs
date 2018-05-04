{-# LANGUAGE DeriveFunctor, OverloadedStrings, TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GADTs #-}
module Declarative where

import Control.Monad.Free

import ALaCarte
import Observable

data ContractF next
  = Zero
  | One Currency
  | Give next
  | And next next
  | Or next next
  | Scale (Obs Int) next
  deriving (Functor)

data OriginalF next
  = Truncate Time next
  | Then next next
  | Get next
  | Anytime next
  deriving (Functor)

data ExtendedF next
  = Cond (Obs Bool) next next
  | When (Obs Bool) next
  | AnytimeO (Obs Bool) next
  | Until (Obs Bool) next
  deriving (Functor)

data MonadicF next
  = GetInt (Int -> next)
  | SetInt Int next
--  | GetIntObs String (Int -> next)
  deriving (Functor)

type Contract = Free (ContractF :+ OriginalF :+ ExtendedF) ()
type ContractM = Free (ContractF :+ OriginalF :+ ExtendedF :+ MonadicF) ()

data Horizon = Time Time | Infinite deriving (Eq, Ord, Show)

data Currency = GBP | USD | EUR deriving (Eq, Show)

zero' :: Contract
zero' = inject Zero

zeroM :: (ContractF :<: f) => Free f ()
zeroM = inject Zero

oneM :: (ContractF :<: f) => Currency -> Free f ()
oneM k = inject (One k)

one' :: Currency -> Contract
one' k = inject (One k)

give' :: Contract -> Contract
give' c = inject (Give c)

giveM :: (ContractF :<: f) => Free f ()
giveM = inject (Give (Pure ()))

and' :: Contract -> Contract -> Contract
and' c1 c2 = inject (And c1 c2)

or' :: Contract -> Contract -> Contract
or' c1 c2 = inject (Or c1 c2)

truncate' :: Time -> Contract -> Contract
truncate' t c = inject (Truncate t c)

truncateM :: (OriginalF :<: f) => Time -> Free f ()
truncateM t = inject (Truncate t (Pure ()))

then' :: Contract -> Contract -> Contract
then' c1 c2 = inject (Then c1 c2)

scale' :: Obs Int -> Contract -> Contract
scale' n c = inject (Scale n c)

scaleM :: (ContractF :<: f) => Obs Int -> Free f ()
scaleM o = inject (Scale o (Pure ()))

scaleK' :: Int -> Contract -> Contract
scaleK' n c = inject (Scale (Constant n) c)

scaleKM :: (ContractF :<: f) => Int -> Free f ()
scaleKM n = inject (Scale (Constant n) (Pure ()))

get' :: Contract -> Contract
get' c = inject (Get c)

getM :: (OriginalF :<: f) => Free f ()
getM = inject (Get (Pure ()))

anytime' :: Contract -> Contract
anytime' c = inject (Anytime c)

anytimeM :: (OriginalF :<: f) => Free f ()
anytimeM = inject (Anytime (Pure ()))

cond' :: Obs Bool -> Contract -> Contract -> Contract
cond' o c1 c2 = inject (Cond o c1 c2)

when' :: Obs Bool -> Contract -> Contract
when' o c = inject (When o c)

whenM :: (ExtendedF :<: f) => Obs Bool -> Free f ()
whenM o = inject (When o (Pure ()))

anytimeO' :: Obs Bool -> Contract -> Contract
anytimeO' o c = inject (AnytimeO o c)

anytimeOM :: (ExtendedF :<: f) => Obs Bool -> Free f ()
anytimeOM o = inject (AnytimeO o (Pure ()))

until' :: Obs Bool -> Contract -> Contract
until' o c = inject (Until o c)

untilM :: (ExtendedF :<: f) => Obs Bool -> Free f ()
untilM o = inject (Until o (Pure ()))

getIntM :: (MonadicF :<: f) => Free f Int
getIntM = inject (GetInt Pure)

setIntM :: (MonadicF :<: f) => Int -> Free f ()
setIntM i = inject (SetInt i (Pure ()))

zcbOriginal :: Time -> Int -> Currency -> Contract
zcbOriginal t x k = scaleK' x (get' (truncate' t (one' k)))

zcbOriginalM :: Time -> Int -> Currency -> Contract
zcbOriginalM t x k = do
  scaleKM x
  getM
  truncateM t
  oneM k

zcbExample :: Contract
zcbExample = zcbOriginal 1 10 GBP

effectfulExample :: ContractM
effectfulExample = do
  setIntM 5
  scaleBy <- getIntM
  scaleKM scaleBy
  oneM GBP