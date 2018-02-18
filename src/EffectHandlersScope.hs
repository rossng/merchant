{-# LANGUAGE TypeOperators, DeriveFunctor #-}

module EffectHandlersScope where

data Backtr a
  = Return a
  | Fail
  | (Backtr a) :& (Backtr a)
  deriving (Functor, Show)

instance Applicative Backtr where
  pure a = undefined
  (<*>) = undefined

instance Monad Backtr where
  return a = Return a
  Return a >>= r = r a
  Fail >>= r = Fail
  (p :& q) >>= r = (p >>= r) :& (q >>= r)

knapsack :: Int -> [Int] -> Backtr [Int]
knapsack w vs | w < 0 = Fail
              | w == 0 = return []
              | w > 0 = do v <- select vs
                           vs' <- knapsack (w-v) vs
                           return (v:vs')

select :: [a] -> Backtr a
select = foldr (:&) Fail . map Return

allsols :: Backtr a -> [a]
allsols (Return a) = [a]
allsols (Fail) = []
allsols (p :& q) = allsols p ++ allsols q