{-# LANGUAGE DeriveFunctor, OverloadedStrings, TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverlappingInstances, GADTs #-}
module Declarative where

import Control.Monad.Free
import Control.Lens

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

type Contract = Free (ContractF :+ OriginalF :+ ExtendedF) ()

data Horizon = Time Time | Infinite deriving (Eq, Ord, Show)

data Currency = GBP | USD | EUR deriving (Eq, Show)

zero' :: Contract
zero' = inject Zero

one' :: Currency -> Contract
one' k = inject (One k)

give' :: Contract -> Contract
give' c = inject (Give c)

and' :: Contract -> Contract -> Contract
and' c1 c2 = inject (And c1 c2)

or' :: Contract -> Contract -> Contract
or' c1 c2 = inject (Or c1 c2)

truncate' :: Time -> Contract -> Contract
truncate' t c = inject (Truncate t c)

then' :: Contract -> Contract -> Contract
then' c1 c2 = inject (Then c1 c2)

scale' :: Obs Int -> Contract -> Contract
scale' n c = inject (Scale n c)

scaleK' :: Int -> Contract -> Contract
scaleK' n c = inject (Scale (Constant n) c)

get' :: Contract -> Contract
get' c = inject (Get c)

anytime' :: Contract -> Contract
anytime' c = inject (Anytime c)

cond' :: Obs Bool -> Contract -> Contract -> Contract
cond' o c1 c2 = inject (Cond o c1 c2)

when' :: Obs Bool -> Contract -> Contract
when' o c = inject (When o c)

anytimeO' :: Obs Bool -> Contract -> Contract
anytimeO' o c = inject (AnytimeO o c)

until' :: Obs Bool -> Contract -> Contract
until' o c = inject (Until o c)

zcb :: Time -> Int -> Currency -> Contract
zcb t x k = scaleK' x (get' (truncate' t (one' k)))

zcbExample :: Contract
zcbExample = zcb 1 10 GBP