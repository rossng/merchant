{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, DeriveFunctor #-}

module ALaCarte where

import Control.Monad.Free

infixr 5 :+
data (f :+ g) e = L (f e) | R (g e) deriving Functor

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance {-# OVERLAPPABLE #-} Functor f => f :<: f where
  inj = id

instance {-# OVERLAPPABLE #-} (Functor f, Functor g) => f :<: (f :+ g) where
  inj = L

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+ g) where
  inj = R . inj

inject :: (g :<: f) => g (Free f a) -> Free f a
inject = Free . inj

handle :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
handle gen _ (Pure x) = gen x
handle gen alg (Free x) = alg (fmap (handle gen alg) x)