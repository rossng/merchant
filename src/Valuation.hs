{-# LANGUAGE OverloadedStrings, GADTs, TypeOperators #-}
module Valuation where

import Declarative
import ALaCarte

import qualified Data.GraphViz.Attributes as A
import qualified Data.GraphViz.Attributes.Complete as C
import Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Commands

import qualified Data.Text.Lazy as T

import Numeric
import Control.Monad.State
import Control.Monad.Free
import Control.Lens

-- this code is from Netrium: https://github.com/netrium/Netrium/blob/master/src/Valuation.hs
-- Netrium is Copyright Anthony Waite, Dave Hetwett, Shaun Laurens 2009-2015, and files herein are licensed
-- under the MIT license.

-- | A 'value process' - a partial function from time to a random variable.
-- | Value processes are lazy and may be infinite.
newtype PR a = PR { unPr :: [RV a] } deriving Show

-- | A random variable - items of the list are the possible values that the RV can take
type RV a = [a]

-- | Truncate a value process.
takePr :: Int -> PR a -> PR a
takePr n (PR rvs) = PR $ take n rvs

-- | Drop values from the beginning of a value process.
dropPr :: Int -> PR a -> PR a
dropPr n (PR rvs) = PR $ drop n rvs

-- | Determines the number of time steps in a value process. Non-terminating for infinite processes.
horizonPr :: PR a -> Int
horizonPr (PR rvs) = length rvs

-- | True iff every value in a boolean value process is True. Non-terminating for infinite processes.
andPr :: PR Bool -> Bool
andPr (PR rvs) = all and rvs

condPr :: PR Bool -> PR a -> PR a -> PR a
condPr = lift3Pr (\b pr1 pr2 -> if b then pr1 else pr2)

-- | Lift single-argument functions to work on value processes
liftPr :: (a -> b) -> PR a -> PR b
liftPr f (PR a) = PR $ map (map f) a

-- | Lift two-argument functions to work on value processes.
lift2Pr :: (a -> b -> c) -> PR a -> PR b -> PR c
lift2Pr f (PR a) (PR b) = PR $ zipWith (zipWith f) a b

-- | Lift three-argument functions to work on value processes.
lift3Pr :: (a -> b -> c -> d) -> PR a -> PR b -> PR c -> PR d
lift3Pr f (PR a) (PR b) (PR c) = PR $ zipWith3 (zipWith3 f) a b c

-- | Lift binary operators to work on value processes. If lists are different length, keeps the original values
-- | where there is only one argument available.
lift2PrAll :: (a -> a -> a) -> PR a -> PR a -> PR a
lift2PrAll f (PR a) (PR b) = PR $ zipWithAll (zipWith f) a b

-- | A version of `zipWith` which works on lists of differing length.
zipWithAll :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithAll f (x:xs) (y:ys) = f x y : zipWithAll f xs ys
zipWithAll _ xs@(_:_) [] = xs
zipWithAll f [] ys@(_:_) = ys
zipWithAll _ _ _ = []

-- | Implement numerical operations element-wise on value processes.
instance Num a => Num (PR a) where
  fromInteger i = bigK (fromInteger i)
  (+) = lift2PrAll (+)
  (-) = lift2PrAll (-)
  (*) = lift2PrAll (*)
  abs = liftPr abs
  signum = liftPr signum

-- | TODO: implement partial ordering instance

instance Eq a => Eq (PR a) where
  (PR a) == (PR b) = a == b

data Model = Model {
  modelStart :: Time, -- ^ Start time for the model
  modelExch :: Currency -> Currency -> PR Double -- ^ Exchange rate evolution model
}

simpleModel :: Time -> Model
simpleModel modelDate = Model {
    modelStart = modelDate,
    modelExch = exchModel
  }
  where exchModel :: Currency -> Currency -> PR Double
        exchModel k1 k2 = rateModel k1 -- TODO

-- | Get the exchange rate model (`exch`) for a given currency
rateModel :: Currency -> PR Double
rateModel k = case k of
  GBP -> rates 1 0.1
  USD -> rates 0.7 0.1
  EUR -> rates 0.9 0.1
  where
    -- | Construct a lattice of possible interest rates given the starting rate and per-timestep increment.
    -- | NB: not realistic!
    rates :: Double -> Double -> PR Double
    rates rateNow delta = PR $ makeRateSlices rateNow 1
      where makeRateSlices rateNow' step = rateSlice rateNow' step : makeRateSlices (rateNow' - delta) (step + 1)
            rateSlice minRate n = take n [minRate, minRate + (delta * 2) ..]

bigK :: a -> PR a
bigK x = PR (konstSlices x)

konstSlices :: t -> [[t]]
konstSlices x = [x] : restSlices [x]
  where
    restSlices slice = (x : slice) : restSlices (x : slice)

-- | Calculate the previous slice in a lattice by averaging pairs of adjacent values in the given slice
prevSlice :: RV Double -> RV Double
prevSlice [] = []
prevSlice [_] = []
prevSlice (n1:n2:rest) = (n1 + n2) / 2 : prevSlice (n2:rest)

disc :: Currency -> (PR Bool, PR Double) -> PR Double
disc k (PR booleanProcess, PR valueProcess) = PR $ disc' booleanProcess valueProcess rateProcess
  where rateProcess = unPr $ rateModel k
        disc' :: [RV Bool] -> [RV Double] -> [RV Double] -> [RV Double]
        disc' (b:bs) (p:ps) (r:rs) = if and b then [p]
          else let rest@(nextSlice:_) = disc' bs ps rs
                   -- discount each possibility of the RV wrt the interest rate
                   slice = zipWith (\x r -> x / (1 + r / 100)) (prevSlice nextSlice) r
                   -- discount only if condition RV is false TODO: correct?
                   slice' = zipWith3 (\b p q -> if b then p else q) b p slice
               in slice' : rest

absorb :: Currency -> (PR Bool, PR Double) -> PR Double
absorb _ (PR booleanProcess, PR valueProcess) = PR $ zipWith (zipWith $ \o p -> if o then 0 else p) booleanProcess valueProcess

snell :: Currency -> (PR Bool, PR Double) -> PR Double
snell = undefined

-- | Every node in the lattice for a value process has an associated probability - the percentage of paths from the root
-- | which pass through it
probabilityLattice :: [RV Double]
probabilityLattice = probabilities pathCounts
  where
    paths :: RV Integer -> [RV Integer]
    paths slice = slice : paths (zipWithAll (+) (0 : slice) slice)
    pathCounts :: [RV Integer]
    pathCounts = paths [1]
    probabilities :: [RV Integer] -> [RV Double]
    probabilities (slice:slices) = map (\n -> fromInteger n / sliceTotal) slice : probabilities slices
      where
        sliceTotal = fromInteger (sum slice)
    probabilities [] = []

expectedValue :: RV Double -> RV Double -> Double
expectedValue outcomes probabilities = sum $ zipWith (*) outcomes probabilities

expectedValuePr :: PR Double -> [Double]
expectedValuePr (PR rvs) = zipWith expectedValue rvs probabilityLattice

--------------------
-- lattice visualisation

renderPrToGraph :: Show a => PR a -> FilePath -> IO FilePath
renderPrToGraph pr = runGraphviz (prToGraph pr) Png

prToGraph :: Show a => PR a -> G.DotGraph Int
prToGraph (PR pr) = digraph (Str "lattice") $ do
    graphAttrs [C.RankDir C.FromLeft]
    addSlices pr'
    joinAllSlices pr'
  where pr' = assignIds pr 0

addSlices :: Show a => [RV (Int, a)] -> DotM Int ()
addSlices [] = return ()
addSlices (rv:rvs) = do
  addSlice rv
  addSlices rvs

addSlice :: Show a => RV (Int, a) -> DotM Int ()
addSlice [] = return ()
addSlice ((n,label):vs) = do
  node n [A.textLabel (T.pack $ show label)]
  addSlice vs

-- | Number each of the nodes in a lattice
assignIds :: [RV a] -> Int -> [RV (Int, a)]
assignIds [] _ = []
assignIds (rv:rvs) n = rv' : assignIds rvs (n + length rv)
  where rv' = zip [n..] rv

joinAllSlices :: [RV (Int, a)] -> DotM Int ()
joinAllSlices [] = return ()
joinAllSlices [_] = return ()
joinAllSlices (a:b:rvs) = do
  joinSlices a b
  joinAllSlices (b:rvs)

-- | Join two slices from a lattice
joinSlices :: RV (Int, a) -> RV (Int, a) -> DotM Int ()
joinSlices fst snd = do
  joinSlices' fst (init snd)
  joinSlices' fst (tail snd)
  where joinSlices' :: RV (Int, a) -> RV (Int, a) -> DotM Int ()
        joinSlices' = zipWithM_ (\ (x, _) (y, _) -> x --> y)

--------------------
-- own algebra

fromIntegralPR :: (Integral a, Num b) => PR a -> PR b
fromIntegralPR pr = PR $ fmap (fmap fromIntegral) (unPr pr)

class Functor f => ValuationAlg f where
  valuationAlg :: Model -> f (PR Double) -> PR Double

instance ValuationAlg ContractF where
  valuationAlg m Zero = bigK 0
  valuationAlg m (One k) = (modelExch m) GBP k
  valuationAlg m (Give pr) = liftPr negate pr
  valuationAlg m (And pr1 pr2) = lift2Pr (+) pr1 pr2
  valuationAlg m (Or pr1 pr2) = lift2PrAll max pr1 pr2
  valuationAlg m (Scale o pr) = lift2Pr (*) (liftPr fromIntegral o') pr
    where o' = evalObs (modelStart m) o

instance (ValuationAlg f, ValuationAlg g) => ValuationAlg (f :+ g) where
  valuationAlg m (L x) = valuationAlg m x
  valuationAlg m (R y) = valuationAlg m y

value :: Time -> Contract -> PR Double
value t (Pure _) = 0
value t c = handle (const 0) (valuationAlg (simpleModel t)) c

instance ValuationAlg OriginalF where
  valuationAlg m (Truncate t pr) = undefined
  valuationAlg m (Then pr1 pr2) = undefined
  valuationAlg m (Get pr) = undefined
  valuationAlg m (Anytime pr) = undefined

instance ValuationAlg ExtendedF where
  valuationAlg m (Cond o pr1 pr2) = undefined
  valuationAlg m (When o pr) = undefined
  valuationAlg m (AnytimeO o pr) = undefined
  valuationAlg m (Until o pr) = undefined

evalObs :: Time -> Obs a -> PR a
evalObs _ (Constant k) = bigK k
evalObs _ (External s) = error "External observable has unknown semantics"
evalObs t (After t') = PR $
  unPr (takePr (t' - t) (bigK False)) ++
  unPr (dropPr (t' - t) (bigK True))

------------------


--takePr :: Int -> PR a -> PR a
--takePr n (PR rvs) = PR $ take n rvs
--
--horizonPr :: PR a -> Int
--horizonPr (PR rvs) = length rvs
--
--andPr :: PR Bool -> Bool
--andPr (PR rvs) = all and rvs
--
--data Model = Model {
--  modelStart :: Time,
--  disc       :: Currency -> (PR Bool, PR Double) -> PR Double,
--  exch       :: Currency -> Currency -> PR Double,
--  absorb     :: Currency -> (PR Bool, PR Double) -> PR Double,
--  rateModel  :: Currency -> PR Double
--}
--
--exampleModel :: CalendarTime -> Model
--exampleModel modelDate = Model {
--  modelStart = (modelDate,0),
--  disc       = disc,
--  exch       = exch,
--  absorb     = absorb,
--  rateModel  = rateModel
--}
--  where
--    rates :: Double -> Double -> PR Double
--    rates rateNow delta = PR $ makeRateSlices rateNow 1
--      where
--        makeRateSlices rateNow n = rateSlice rateNow n : makeRateSlices (rateNow - delta) (n + 1)
--        rateSlice minRate n = take n [minRate,minRate + (delta * 2) ..]
--
--    rateModels = [(EUR, rates 6.5 0.25)
--                , (GBP, rates 8   0.5)
--                , (USD, rates 5   1)]
--
--    rateModel k =
--      fromMaybe (error $ "rateModel: currency not found " ++ show k) (lookup k rateModels)
--
--    disc :: Currency -> (PR Bool, PR Double) -> PR Double
--    disc k (PR bs, PR rs) = PR $ discCalc bs rs (unPr $ rateModel k)
--      where
--        discCalc :: [RV Bool] -> [RV Double] -> [RV Double] -> [RV Double]
--        discCalc (bRv:bs) (pRv:ps) (rateRv:rs) =
--          if and bRv -- test for horizon
--            then [pRv]
--            else let rest@(nextSlice:_) = discCalc bs ps rs
--                     discSlice = zipWith (\x r -> x / (1 + r/100)) (prevSlice nextSlice) rateRv
--                     thisSlice = zipWith3 (\b p q -> if b then p else q) -- allow for partially discounted slices
--                                  bRv pRv discSlice
--              in thisSlice : rest
--
--        prevSlice :: RV Double -> RV Double
--        prevSlice [] = []
--        prevSlice [_] = []
--        prevSlice (n1:rest@(n2:_)) = (n1+n2)/2 : prevSlice rest
--

--
--datePr :: PR Time
--datePr = PR $ timeSlices [time0]
--  where timeSlices sl@((s,t):_) = sl : timeSlices [(s,t+1) | _ <- [0..t+1]]
--
--evalObs :: Observable -> PR a
--evalObs (External s) = undefined
----evalObs ()
--
--valuationAlg :: ContractF (PR Double) -> PR Double
--valuationAlg Zero = bigK 0
--valuationAlg (One k) = undefined
--valuationAlg (Give c) = -c
--valuationAlg (And c1 c2) = undefined
--valuationAlg (Or c1 c2) = undefined
--valuationAlg (Truncate t c) = undefined
--valuationAlg (Then c1 c2) = undefined
--valuationAlg (Scale n c) = undefined
--valuationAlg (Get c) = undefined
--valuationAlg (Anytime c) = undefined