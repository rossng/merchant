{-# LANGUAGE TypeOperators, OverlappingInstances, DeriveFunctor, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Merchant where

import GHC.Generics
import Control.Monad (ap)

data Zero e = Zero deriving (Functor, Show)
data One e = One deriving (Functor, Show)
data Give e = Give e deriving (Functor, Show)
data And e = And e e deriving (Functor, Show)
data Or e = Or e e deriving (Functor, Show)

zero :: (Zero :<: f) => Free f a
zero = inject Zero

one :: (One :<: f) => Free f a
one = inject One

give :: (Give :<: f) => Free f a -> Free f a
give c = inject (Give c)

and :: (And :<: f) => Free f a -> Free f a -> Free f a
c1 `and` c2 = inject (And c1 c2)

or :: (Or :<: f) => Free f a -> Free f a -> Free f a
c1 `or` c2 = inject (Or c1 c2)

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
  render One = "one"

instance Render Give where
  render (Give c) = "(give " ++ c ++ ")"

instance Render And where
  render (And c1 c2) = "(" ++ c1 ++ " `and` " ++ c2 ++ ")"

instance Render Or where
  render (Or c1 c2) = "(" ++ c1 ++ " `or` " ++ c2 ++ ")"

----

type Contract = Free (Zero :+: One :+: Give :+: And)

exampleContract :: Contract ()
exampleContract = give zero