{-# LANGUAGE QuasiQuotes, OverloadedStrings, TypeOperators, TemplateHaskell, GADTs #-}

module SolidityObservable where

import NeatInterpolation (text)
import qualified Data.Text as T
import Control.Monad.State
import TextShow
import Control.Lens
import qualified Data.List.Index as IList

import Control.Monad.Free
import Declarative
import ALaCarte
import Observable

data SolidityObs = OBExternal String
                 | OBConstant Bool
                 | OBAfter Time
                 | OBAnd Int Int
                 | OBGreaterThan Int Int
                 | OIExternal String
                 | OIConstant Int
                 | OISubtract Int Int
                 deriving Show

type ObsGraph = [SolidityObs]

class Solidifiable a where
  addToObsGraph :: Obs a -> State ObsGraph Int

instance Solidifiable Bool where
  addToObsGraph (External addr) = do
    idx <- length <$> get
    modify (++ [OBExternal addr])
    return idx
  addToObsGraph (Constant value) = do
    idx <- length <$> get
    modify (++ [OBConstant value])
    return idx
  addToObsGraph (After time) = do
    idx <- length <$> get
    modify (++ [OBAfter time])
    return idx
  addToObsGraph (OAnd o1 o2) = do
    idx1 <- addToObsGraph o1
    idx2 <- addToObsGraph o2
    idx <- length <$> get
    modify (++ [OBAnd idx1 idx2])
    return idx
  addToObsGraph (OGreaterThan o1 o2) = do
    idx1 <- addToObsGraph o1
    idx2 <- addToObsGraph o2
    idx <- length <$> get
    modify (++ [OBGreaterThan idx1 idx2])
    return idx

instance Solidifiable Int where
  addToObsGraph (External addr) = do
    idx <- length <$> get
    modify (++ [OIExternal addr])
    return idx
  addToObsGraph (Constant value) = do
    idx <- length <$> get
    modify (++ [OIConstant value])
    return idx
  addToObsGraph (OSubtract o1 o2) = do
    idx1 <- addToObsGraph o1
    idx2 <- addToObsGraph o2
    idx <- length <$> get
    modify (++ [OISubtract idx1 idx2])
    return idx

obsToGraph :: Solidifiable a => Obs a -> State ObsGraph Int
obsToGraph = addToObsGraph


data ObsCompileState = ObsCompileState {
  _source :: [T.Text],
  _counter :: Int
}
makeLenses ''ObsCompileState

initialCompileState :: ObsCompileState
initialCompileState = ObsCompileState {
  _source = [],
  _counter = 0
}

compileObsGraph :: ObsGraph -> Int -> T.Text
compileObsGraph obsGraph rootIdx = evalState state initialCompileState
  where state = compileSolidityObs obsGraph (obsGraph !! rootIdx)

addClass cls sources = cls : sources

compileSolidityObs :: ObsGraph -> SolidityObs -> State ObsCompileState T.Text
compileSolidityObs obsGraph (OBExternal addr) = return ""
compileSolidityObs obsGraph (OBConstant value) = do
  counter %= (+1)
  n <- use counter
  source %= (addClass $ constantBoolS value n)
  return ("ConstantBoolObservable_" `T.append` showt n)

constantBoolS :: Bool -> Int -> T.Text
constantBoolS value idx = [text|
  contract ConstantBoolObservable_${idx'} is BoolObservable {
      BoolObservable.Value[] public valueHistory_;

      constructor() public {
          valueHistory_.push(BoolObservable.Value(${value'}, 0));
      }

      function getValueHistory() public returns(BoolObservable.Value[]) {
          return valueHistory_;
      }

      function getValue() public view returns(bool) {
          return valueHistory_[0].value;
      }

      function getTimestamp() public view returns(uint) {
          return valueHistory_[0].timestamp;
      }

      function getFirstSince(function(bool) external pure returns(bool) condition, uint) public returns(bool, uint) {
          if (condition(valueHistory_[0].value)) {
              return (true, 0);
          } else {
              return (false, 0);
          }
      }
  }
  |]
  where value' = case value of
          True -> "true"
          False -> "false"
        idx' = showt idx