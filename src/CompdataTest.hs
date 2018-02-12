{-# LANGUAGE TemplateHaskell, TypeOperators, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, UndecidableInstances, ConstraintKinds, DeriveFunctor, OverlappingInstances #-}


module CompdataTest where

import Data.Comp
import Data.Comp.Show ()
import Data.Comp.Derive

-- Signature for values and operators
data Value a = Const Int | Pair a a
  deriving Functor
data Op a    = Add a a | Mult a a | Fst a | Snd a
  deriving Functor

-- Signature for the simple expression language
type Sig = Op :+: Value

-- Derive boilerplate code using Template Haskell
$(derive [makeTraversable, makeFoldable,
          makeEqF, makeShowF, smartConstructors, smartAConstructors]
          [''Value, ''Op])

-- Term evaluation algebra
class Eval f v where
  evalAlg :: Alg f (Term v)

$(derive [liftSum] [''Eval])

-- Lift the evaluation algebra to a catamorphism
eval :: (Functor f, Eval f v) => Term f -> Term v
eval = cata evalAlg

instance (f :<: v) => Eval f v where
  evalAlg = inject -- default instance

instance (Value :<: v) => Eval Op v where
  evalAlg (Add x y)  = iConst $ projC x + projC y
  evalAlg (Mult x y) = iConst $ projC x * projC y
  evalAlg (Fst x)    = fst $ projP x
  evalAlg (Snd x)    = snd $ projP x

projC :: (Value :<: v) => Term v -> Int
projC v = case project v of Just (Const n) -> n

projP :: (Value :<: v) => Term v -> (Term v, Term v)
projP v = case project v of Just (Pair x y) -> (x,y)

-- Example: evalEx = iConst 5
evalEx :: Term Value
evalEx = eval (iConst 1 `iAdd` (iConst 2 `iMult` iConst 2) :: Term Sig)