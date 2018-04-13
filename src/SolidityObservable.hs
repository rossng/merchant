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

data SType = SInt | SBool
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
  _obsSource :: [T.Text],
  _obsCounter :: Int
}
makeLenses ''ObsCompileState

initialCompileState :: ObsCompileState
initialCompileState = ObsCompileState {
  _obsSource = [],
  _obsCounter = 0
}

compileObs :: Solidifiable a => Obs a -> (T.Text, T.Text)
compileObs obs = compileObsGraph graph rootIdx
  where (rootIdx, graph) = runState (obsToGraph obs) []

compileObsGraph :: ObsGraph -> Int -> (T.Text, T.Text)
compileObsGraph obsGraph rootIdx = (T.unlines $ compileState ^. obsSource, className)
  where state = compileSolidityObs obsGraph (obsGraph !! rootIdx)
        (className, compileState) = runState state initialCompileState

addClass cls sources = cls : sources

compileSolidityObs :: ObsGraph -> SolidityObs -> State ObsCompileState T.Text
compileSolidityObs obsGraph (OBExternal addr) = return [text|BoolObservable(${addr'})|]
  where addr' = T.pack addr
compileSolidityObs obsGraph (OBConstant value) = do
  obsCounter += 1
  n <- use obsCounter
  let (name, source) = constantBoolS value n
  obsSource %= addClass source
  return ([text|new ${name}()|])
compileSolidityObs obsGraph (OBAfter time) = do
  obsCounter += 1
  n <- use obsCounter
  let (name, source) = afterBoolS time n
  obsSource %= addClass source
  return ([text|new ${name}()|])
compileSolidityObs obsGraph (OBAnd o1 o2) = do
  constructor1 <- compileSolidityObs obsGraph (obsGraph !! o1)
  constructor2 <- compileSolidityObs obsGraph (obsGraph !! o2)
  obsCounter += 1
  n <- use obsCounter
  let (name, source) = andBoolS constructor1 constructor2 n
  obsSource %= addClass source
  return ([text|new ${name}()|])
compileSolidityObs obsGraph (OBGreaterThan o1 o2) = do
  constructor1 <- compileSolidityObs obsGraph (obsGraph !! o1)
  constructor2 <- compileSolidityObs obsGraph (obsGraph !! o2)
  obsCounter += 1
  n <- use obsCounter
  let (name, source) = greaterThanBoolS constructor1 constructor2 n
  obsSource %= addClass source
  return ([text|new ${name}()|])
compileSolidityObs obsGraph (OIExternal addr) = return [text|IntObservable(${addr'})|]
  where addr' = T.pack addr
compileSolidityObs obsGraph (OIConstant value) = do
  obsCounter += 1
  n <- use obsCounter
  let (name, source) = constantIntS value n
  obsSource %= addClass source
  return ([text|new ${name}()|])
compileSolidityObs obsGraph (OISubtract o1 o2) = do
  constructor1 <- compileSolidityObs obsGraph (obsGraph !! o1)
  constructor2 <- compileSolidityObs obsGraph (obsGraph !! o2)
  obsCounter += 1
  n <- use obsCounter
  let (name, source) = subtractIntS constructor1 constructor2 n
  obsSource %= addClass source
  return ([text|new ${name}()|])

constantBoolS :: Bool -> Int -> (T.Text, T.Text)
constantBoolS value idx =
  ( [text|ConstantBoolObservable_${idx'}|]
  , [text|
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
  |])
  where
    value' = if value then "true" else "false"
    idx' = showt idx

afterBoolS :: Time -> Int -> (T.Text, T.Text)
afterBoolS time idx =
  ([text|AfterBoolObservable_${idx'}|],
  [text|
  contract AfterBoolObservable_${idx'} is BoolObservable {
      BoolObservable.Value[] valueHistory_;

      constructor() public {
      }

      function getValueHistory() public returns(BoolObservable.Value[]) {
          valueHistory_.length = 0;
          if (block.timestamp < ${time'}) {
              valueHistory_.push(BoolObservable.Value(false, 0));
          } else {
              valueHistory_.push(BoolObservable.Value(false, 0));
              valueHistory_.push(BoolObservable.Value(true, ${time'}));
          }
          return valueHistory_;
      }

      function getValue() public view returns(bool) {
          return block.timestamp < ${time'} ? false : true;
      }

      function getTimestamp() public view returns(uint) {
          return block.timestamp < ${time'} ? 0 : ${time'};
      }

      function getFirstSince(function(bool) external pure returns(bool) condition, uint since) public returns(bool, uint) {
          if (now >= since) {
              if (since < ${time'}) {
                  if (condition(false)) {
                      return (true, since);
                  } else if (now >= ${time'} && condition(true)) {
                      return (true, ${time'});
                  } else {
                      return (false, 0);
                  }
              } else {
                  if (condition(true)) {
                      return (true, since);
                  } else {
                      return (false, 0);
                  }
              }
          }
          return (false, 0);
      }
  }
  |])
  where time' = showt time
        idx' = showt idx

andBoolS :: T.Text -> T.Text -> Int -> (T.Text, T.Text)
andBoolS = binaryS "And" "&&" SBool SBool

greaterThanBoolS :: T.Text -> T.Text -> Int -> (T.Text, T.Text)
greaterThanBoolS = binaryS "GreaterThan" ">" SInt SBool

binaryS :: T.Text -> T.Text -> SType -> SType -> T.Text -> T.Text -> Int -> (T.Text, T.Text)
binaryS opName op inputType outputType constructor1 constructor2 idx =
  ([text|${opName}${outputType'}_${idx'}|],
  [text|
  contract ${opName}${outputType'}_${idx'} is ${outputType'} {
      ${inputType'} b1_;
      ${inputType'} b2_;
      ${outputType'}.Value[] public valueHistory_;

      constructor() public {
          b1_ = ${constructor1};
          b2_ = ${constructor2};
      }

      function getValueHistory() public returns(${outputType'}.Value[]) {
          ${inputType'}.Value[] memory b1 = b1_.getValueHistory();
          ${inputType'}.Value[] memory b2 = b2_.getValueHistory();
          valueHistory_.length = 0;

          uint i = 0;
          uint j = 0;

          // Note: this will fail if both input observables do not start at time 0
          // This is intended behaviour (for now)

          while (i < b1.length && j < b2.length) {
              if (b1[i].timestamp < b2[j].timestamp) {
                  valueHistory_.push(${outputType'}.Value(b1[i].value ${op} b2[j-1].value, b1[i].timestamp));
                  i++;
              } else if (b1[i].timestamp > b2[i].timestamp) {
                  valueHistory_.push(${outputType'}.Value(b1[i-1].value ${op} b2[j].value, b2[j].timestamp));
                  j++;
              } else {
                  valueHistory_.push(${outputType'}.Value(b1[i].value ${op} b2[i].value, b1[i].timestamp));
                  i++;
                  j++;
              }
          }
          while (i < b1.length) {
              valueHistory_.push(${outputType'}.Value(b1[i].value ${op} b2[j-1].value, b1[i].timestamp));
              i++;
          }
          while (j < b2.length) {
              valueHistory_.push(${outputType'}.Value(b1[i-1].value ${op} b2[j].value, b2[j].timestamp));
              j++;
          }

          return valueHistory_;
      }

      function getValue() public view returns(${outputRawType'}) {
          return b1_.getValue() ${op} b2_.getValue();
      }

      function getTimestamp() public view returns(uint) {
          uint b1 = b1_.getTimestamp();
          uint b2 = b2_.getTimestamp();
          return b1 > b2 ? b1 : b2;
      }

      function getFirstSince(function(${outputRawType'}) external pure returns(bool) condition, uint sinceTimestamp) public returns(bool, uint) {
          getValueHistory();
          uint currentTimestamp = 0;
          ${outputRawType'} currentValue = valueHistory_[0].value;
          for (uint i = 0; i < valueHistory_.length; i++) {
              if (valueHistory_[i].timestamp < sinceTimestamp) {
                  currentTimestamp = valueHistory_[i].timestamp;
                  currentValue = valueHistory_[i].value;
                  continue;
              } else {
                  if (condition(currentValue)) {
                      return (true, sinceTimestamp);
                  }
                  currentTimestamp = valueHistory_[i].timestamp;
                  currentValue = valueHistory_[i].value;
                  if (condition(currentValue)) {
                      return (true, currentTimestamp);
                  }
              }
          }
          return (false, 0);
      }
  }
  |])
  where idx' = showt idx
        inputType' = case inputType of
          SBool -> "BoolObservable"
          SInt -> "IntObservable"
        outputType' = case outputType of
          SBool -> "BoolObservable"
          SInt -> "IntObservable"
        outputRawType' = case outputType of
          SBool -> "bool"
          SInt -> "int"

constantIntS :: Int -> Int -> (T.Text, T.Text)
constantIntS value idx =
  ([text|ConstantIntObservable_${idx'}|],
  [text|
  contract ConstantIntObservable_${idx'} is IntObservable {
      IntObservable.Value[] public valueHistory_;

      constructor() public {
          valueHistory_.push(IntObservable.Value(${value'}, 0));
      }

      function getValueHistory() public returns(IntObservable.Value[]) {
          return valueHistory_;
      }

      function getValue() public view returns(int) {
          return valueHistory_[0].value;
      }

      function getTimestamp() public view returns(uint) {
          return valueHistory_[0].timestamp;
      }

      function getFirstSince(function(int) external pure returns(bool) condition, uint) public returns(bool, uint) {
          if (condition(valueHistory_[0].value)) {
              return (true, 0);
          } else {
              return (false, 0);
          }
      }
  }
  |])
  where value' = showt value
        idx' = showt idx


subtractIntS :: T.Text -> T.Text -> Int -> (T.Text, T.Text)
subtractIntS = binaryS "Subtract" "-" SInt SInt

baseObservableS :: T.Text -> T.Text -> T.Text
baseObservableS name typeName =
  [text|
  contract ${name}Observable {
      struct Value {
          ${typeName} value;
          uint timestamp;
      }

      function getValueHistory() public returns(${name}Observable.Value[]);
      function getValue() public view returns(${typeName});
      function getTimestamp() public view returns(uint);

      function getFirstSince(function(${typeName}) external pure returns(bool) condition, uint sinceTimestamp) public returns(bool, uint);
  }
  |]