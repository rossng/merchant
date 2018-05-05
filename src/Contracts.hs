module Contracts where

import Language
import Observable

-- Combinators

zcbOriginal :: Time -> Int -> Currency -> Contract
zcbOriginal t x k = scaleK' x (get' (truncate' t (one' k)))

zcbOriginalM :: Time -> Int -> Currency -> Contract
zcbOriginalM t x k = do
  scaleKM x
  getM
  truncateM t
  oneM k

andGive :: Contract -> Contract -> Contract
andGive c d = c `and'` give' d

europeanOriginal :: Time -> Contract -> Contract
europeanOriginal t u = get' (truncate' t (u `or'` zero'))

perhaps :: Time -> Contract -> Contract
perhaps t u = truncate' t (u `or'` zero')

americanOriginal :: (Time, Time) -> Contract -> Contract
americanOriginal (t1,t2) u = get' (truncate' t1 opt) `then'` opt
  where opt :: Contract
        opt = anytime' (perhaps t2 u)

european :: Time -> Contract -> Contract
european t u = when' (At t) (u `or'` zero')

american :: (Time, Time) -> Contract -> Contract
american (t1, t2) = anytimeO' (between t1 t2)

-- Example Contracts

zcbExample :: Contract
zcbExample = zcbOriginal 1 10 GBP

effectfulExample :: ContractM
effectfulExample = do
  setIntM 5
  scaleBy <- getIntM
  scaleKM scaleBy
  oneM GBP