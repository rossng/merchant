{-# LANGUAGE TypeOperators, FlexibleContexts, DeriveFunctor #-}
module Calculator where

import Control.Monad.Free
import Control.Monad.State
import ALaCarte


data Incr k = Incr Int k deriving Functor
data Recall k = Recall (Int -> k) deriving Functor
data Clear k = Clear k deriving Functor

incr :: (Incr :<: f) => Int -> Free f ()
incr i = inject (Incr i (Pure ()))

recall :: (Recall :<: f) => Free f Int
recall = inject (Recall Pure)

clear :: (Clear :<: f) => Free f ()
clear = inject (Clear (Pure ()))

program :: Free (Recall :+ Incr :+ Clear) Int
program = do incr 2
             y <- recall
             clear
             return y

class Functor f => Run f where
  runAlg :: f (State Int a) -> State Int a

instance Run Incr where
  runAlg (Incr i k) = do
                        mem <- get
                        put (mem + i)
                        k

instance Run Recall where
  runAlg (Recall k) = do
                        mem <- get
                        k mem

instance Run Clear where
  runAlg (Clear k) = do
                        put 0
                        k

instance (Run f, Run g) => Run (f :+ g) where
  runAlg (L x) = runAlg x
  runAlg (R y) = runAlg y

run :: Run f => Free f a -> Int -> (a, Int)
run = runState . handle pure runAlg