{-# LANGUAGE TypeOperators, OverlappingInstances, DeriveFunctor, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Merchant where

import GHC.Generics
import Control.Monad (ap)

data Currency = GBP | USD | EUR deriving Show
newtype Address = Address Int deriving Show
newtype Time = Time Int deriving Show

data Zero e = Zero deriving (Functor, Show)
data One e = One Currency deriving (Functor, Show)
data Give e = Give e deriving (Functor, Show)
data And e = And e e deriving (Functor, Show)
data Or e = Or e e deriving (Functor, Show)
data Then e = Then e e deriving (Functor, Show)
data Scale e = Scale Int e deriving (Functor, Show)
data ScaleObs e = ScaleObs Address e deriving (Functor, Show)
data Timebound e = Timebound Time Time e deriving (Functor, Show)

zero :: (Zero :<: f) => Free f a
zero = inject Zero

one :: (One :<: f) => Currency -> Free f a
one c = inject (One c)

give :: (Give :<: f) => Free f a -> Free f a
give c = inject (Give c)

($&) :: (And :<: f) => Free f a -> Free f a -> Free f a
c1 $& c2 = inject (And c1 c2)

cAnd :: (And :<: f) => Free f a -> Free f a -> Free f a
cAnd = ($&)

($|) :: (Or :<: f) => Free f a -> Free f a -> Free f a
c1 $| c2 = inject (Or c1 c2)

cOr :: (Or :<: f) => Free f a -> Free f a -> Free f a
cOr = ($|)

($>) :: (Then :<: f) => Free f a -> Free f a -> Free f a
c1 $> c2 = inject (Then c1 c2)

cThen :: (Then :<: f) => Free f a -> Free f a -> Free f a
cThen = ($>)

($*) :: (Scale :<: f) => Int -> Free f a -> Free f a
k $* c = inject (Scale k c)

cScale :: (Scale :<: f) => Int -> Free f a -> Free f a
cScale = ($*)

($*~) :: (ScaleObs :<: f) => Address -> Free f a -> Free f a
obs $*~ c = inject (ScaleObs obs c)

cTimebound :: (Timebound :<: f) => Time -> Time -> Free f a -> Free f a
cTimebound t0 t1 c = inject (Timebound t0 t1 c)

----

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = L1

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = R1 . inj

data Free f a = Pure a | Impure (f (Free f a))

inject :: (g :<: f) => g (Free f a) -> Free f a
inject = Impure . inj

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure t) = Impure (fmap (fmap f) t)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  return = Pure
  (Pure x) >>= f = f x
  (Impure t) >>= f = Impure (fmap (>>= f) t)

handle :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
handle gen alg (Pure x) = gen x
handle gen alg (Impure t) = alg (fmap (handle gen alg) t)

----

pretty :: Render f => Free f a -> String
pretty = handle (const "") render

class Functor f => Render f where
  render :: f String -> String

instance (Render f, Render g) => Render (f :+: g) where
  render (L1 x) = render x
  render (R1 x) = render x

instance Render Zero where
  render Zero = "zero"

instance Render One where
  render (One currency) = "(one " ++ show currency ++ ")"

instance Render Give where
  render (Give c) = "(give " ++ c ++ ")"

instance Render And where
  render (And c1 c2) = "(and " ++ c1 ++ " " ++ c2 ++ ")"

instance Render Or where
  render (Or c1 c2) = "(or " ++ c1 ++ " " ++ c2 ++ ")"

instance Render Then where
  render (Then c1 c2) = "(then " ++ c1 ++ " " ++ c2 ++ ")"

instance Render Scale where
  render (Scale k c) = "(scaleK " ++ show k ++ " " ++ c ++ ")"

instance Render ScaleObs where
  render (ScaleObs obs c) = "(scaleObs (" ++ show obs ++ ") " ++ c ++ ")"

instance Render Timebound where
  render (Timebound t0 t1 c) = "(timebound (" ++ show t0 ++ ") (" ++ show t1 ++ ") " ++ c ++ ")"

----

findelise :: Findel f => Free f a -> String
findelise = handle (const "") findel

findelCurrency :: Currency -> String
findelCurrency USD = "USD"
findelCurrency GBP = "GBP"
findelCurrency EUR = "EUR"

class Functor f => Findel f where
  findel :: f String -> String

instance (Findel f, Findel g) => Findel (f :+: g) where
  findel (L1 x) = findel x
  findel (R1 x) = findel x

instance Findel Zero where
  findel Zero = "Zero"

instance Findel One where
  findel (One curr) = "One(EUR)"

instance Findel Give where
  findel (Give c) = "Give(" ++ c ++ ")"

instance Findel And where
  findel (And c1 c2) = "And(" ++ c1 ++ "," ++ c2 ++ ")"

instance Findel Or where
  findel (Or c1 c2) = "Or(" ++ c1 ++ "," ++ c2 ++ ")"

instance Findel Scale where
  findel (Scale k c) = "Scale(" ++ show k ++ "," ++ c ++ ")"

instance Findel ScaleObs where
  findel (ScaleObs (Address addr) c) = "ScaleObs(" ++ show addr ++ "," ++ c ++ ")"

instance Findel Timebound where
  findel (Timebound (Time t1) (Time t2) c) = "Timebound(" ++ show t1 ++ "," ++ show t2 ++ "," ++ c ++ ")"

----

type Contract = Free (Zero :+: One :+: Give :+: And :+: Or :+: Scale :+: ScaleObs :+: Timebound)

exampleContract :: Contract ()
exampleContract = cTimebound (Time 0) (Time 4) $ Address 5 $*~ (one GBP $| give zero)

renderedContract :: String
renderedContract = pretty exampleContract

findelContract :: String
findelContract = findelise exampleContract