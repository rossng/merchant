{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GADTs #-}
module Observable where

type Time = Int

data Obs a where
  External :: String -> Obs a
  Constant :: a -> Obs a
  After :: Time -> Obs Bool
  Before :: Time -> Obs Bool
  At :: Time -> Obs Bool
  OAnd :: Obs Bool -> Obs Bool -> Obs Bool
  OGreaterThan :: Obs Int -> Obs Int -> Obs Bool
  OSubtract :: Obs Int -> Obs Int -> Obs Int

between :: Time -> Time -> Obs Bool
between t1 t2 = OAnd (After t1) (Before t2)

-- TODO: get time with now :: IO Time