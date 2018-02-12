{-# LANGUAGE DeriveFunctor #-}
module RedditCC where

import Control.Monad
import Control.Monad.Free

data Currency = USD | GBP | EUR | ZAR | KYD | CHF  deriving (Eq, Show)
type Date = (CalendarTime, TimeStep)
type TimeStep = Int
type CalendarTime = ()
newtype PR a = PR { unPr :: [RV a] } deriving Show
type RV a = [a]
newtype Obs a = Obs (Date -> PR a)

mkDate :: TimeStep -> Date
mkDate s = ((),s)

time0 :: Date
time0 = mkDate 0

instance Show a => Show (Obs a) where
    show (Obs o) = let (PR (rv:_)) = o time0 in "(Obs " ++ show rv ++ ")"

data ContractF x
    = Zero
    | One  Currency
    | Give x
    | And  x x
    | Or   x x
    | Cond    (Obs Bool)   x x
    | Scale   (Obs Double) x
    | When    (Obs Bool)   x
    | Anytime (Obs Bool)   x
    | Until   (Obs Bool)   x
    deriving (Show, Functor)

type Contract = Free ContractF

zero :: Contract a
zero = liftF Zero

one :: Currency -> Contract a
one currency = liftF (One currency)

give :: Contract ()
give = liftF (Give ())

cAnd :: Contract Bool
cAnd = liftF (And False True)

cOr :: Contract Bool
cOr = liftF (Or False True)

cond :: Obs Bool -> Contract Bool
cond obs = liftF (Cond obs False True)

scale :: Obs Double -> Contract ()
scale obs = liftF (Scale obs ())

cWhen :: Obs Bool -> Contract ()
cWhen obs = liftF (When obs ())

anytime :: Obs Bool -> Contract ()
anytime obs = liftF (Anytime obs ())

cUntil :: Obs Bool -> Contract ()
cUntil obs = liftF (Until obs ())