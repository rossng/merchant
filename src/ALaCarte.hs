{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, OverlappingInstances #-}

module ALaCarte where

newtype Fix f = In (f (Fix f))

data Val e = Val Int
data Add e = Add e e
data Mul e = Mul e e

infixr 5 :+
data (f :+ g) e = L (f e) | R (g e)

instance Functor Val where
  fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add l r) = Add (f l) (f r)

instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)

instance (Functor f, Functor g) => Functor (f :+ g) where
  fmap f (L x) = L (fmap f x)
  fmap f (R x) = R (fmap f x)

inOp :: Fix f -> f (Fix f)
inOp (In o) = o

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . inOp

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

instance (Eval f, Eval g) => Eval (f :+ g) where
  evalAlgebra (L x) = evalAlgebra x
  evalAlgebra (R y) = evalAlgebra y

eval :: Eval f => Fix f -> Int
eval = cata evalAlgebra

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g) => f :<: (f :+ g) where
  inj = L

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+ g) where
  inj = R . inj

inject :: (g :<: f) => g (Fix f) -> Fix f
inject = In . inj

val :: (Val :<: f) => Int -> Fix f
val x = inject (Val x)

infixl 6 ~+
(~+) :: (Add :<: f) => Fix f -> Fix f -> Fix f
x ~+ y = inject (Add x y)

infixl 7 ~*
(~*) :: (Mul :<: f) => Fix f -> Fix f -> Fix f
x ~* y = inject (Mul x y)

someVal = In (Val 5)
someVal' :: Fix (Val)
someVal' = val 5

someAdd :: Fix (Val :+ Add)
someAdd = In (R (Add (In (L (Val 10))) (In (L (Val 15)))))
someAdd' :: Fix (Val :+ Add)
someAdd' = val 10 ~+ val 15

someMul :: Fix (Val :+ Add :+ Mul)
someMul = val 5 ~+ val 8 ~* val 4

class Render f where
  render :: Render g => f (Fix g) -> String

pretty :: Render f => Fix f -> String
pretty (In t) = render t

instance Render Val where
  render (Val i) = show i

instance Render Add where
  render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"

instance Render Mul where
  render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"

instance (Render f, Render g) => Render (f :+ g) where
  render (L x) = render x
  render (R y) = render y

data Free f a = Pure a | Impure (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure t) = Impure (fmap (fmap f) t)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> y = fmap f y
  Impure f <*> y = undefined

instance Functor f => Monad (Free f) where
  return = Pure
  (Pure x) >>= f = f x
  (Impure t) >>= f = Impure (fmap (>>= f) t)
