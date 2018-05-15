{-# LANGUAGE QuasiQuotes, OverloadedStrings, TypeOperators, TemplateHaskell, GADTs #-}

module SolidityObservable where

import NeatInterpolation (text)
import qualified Data.Text as T
import Control.Monad.State
import TextShow
import Control.Lens

import Observable

data SType = SInt | SBool

data ObsCompileState = ObsCompileState {
  _obsSource :: [T.Text],
  _obsNames :: [(SType, T.Text)]
}
makeLenses ''ObsCompileState

initialCompileState :: ObsCompileState
initialCompileState = ObsCompileState {
  _obsSource = [],
  _obsNames = []
}

getObsIndex = length <$> use obsNames

class Solidifiable a where
  compileObs :: Obs a -> State ObsCompileState Int

instance Solidifiable Bool where
  compileObs (External addr) = do
    n <- getObsIndex
    obsNames %= (|> (SBool, [text|BoolObservable(${addr'})|]))
    return n
    where addr' = T.pack addr
  compileObs (Constant value) = do
    n <- getObsIndex
    let (name, source) = constantBoolS value n
    obsNames %= (|> (SBool, [text|new ${name}(this)|]))
    obsSource %= addClass source
    return n
  compileObs (After time) = do
    n <- getObsIndex
    let (name, source) = afterBoolS time n
    obsNames %= (|> (SBool, [text|new ${name}(this)|]))
    obsSource %= addClass source
    return n
  compileObs (Before time) = do
    n <- getObsIndex
    let (name, source) = beforeBoolS time n
    obsNames %= (|> (SBool, [text|new ${name}(this)|]))
    obsSource %= addClass source
    return n
  compileObs (OAnd o1 o2) = do
   obsIndex1 <- compileObs o1
   obsIndex2 <- compileObs o2
   n <- getObsIndex
   let (name, source) = andBoolS obsIndex1 obsIndex2 n
   obsNames %= (|> (SBool, [text|new ${name}(this)|]))
   obsSource %= addClass source
   return n
  compileObs (OGreaterThan o1 o2) = do
   obsIndex1 <- compileObs o1
   obsIndex2 <- compileObs o2
   n <- getObsIndex
   let (name, source) = andBoolS obsIndex1 obsIndex2 n
   obsNames %= (|> (SBool, [text|new ${name}(this)|]))
   obsSource %= addClass source
   return n
  compileObs (At time) = do
    n <- getObsIndex
    let (name, source) = atBoolS time n
    obsNames %= (|> (SBool, [text|new ${name}(this)|]))
    obsSource %= addClass source
    return n

instance Solidifiable Int where
  compileObs (External addr) = do
    n <- getObsIndex
    obsNames %= (|> (SInt, [text|IntObservable(${addr'})|]))
    return n
    where addr' = T.pack addr
  compileObs (Constant value) = do
    n <- getObsIndex
    let (name, source) = constantIntS value n
    obsNames %= (|> (SInt, [text|new ${name}(this)|]))
    obsSource %= addClass source
    return n
  compileObs (OSubtract o1 o2) = do
    obsIndex1 <- compileObs o1
    obsIndex2 <- compileObs o2
    n <- getObsIndex
    let (name, source) = subtractIntS obsIndex1 obsIndex2 n
    obsNames %= (|> (SInt, [text|new ${name}(this)|]))
    obsSource %= addClass source
    return n

addClass :: a -> [a] -> [a]
addClass cls sources = cls : sources

constantBoolS :: Bool -> Int -> (T.Text, T.Text)
constantBoolS value idx =
  ( [text|ConstantBoolObservable_${idx'}|]
  , [text|
  contract ConstantBoolObservable_${idx'} is BoolObservable {
      BoolObservable.Value[] public valueHistory_;

      constructor(ObsWrapper wrapper) public BoolObservable(wrapper) {
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

      function getFirstSince(function(bool) external pure returns(bool) condition, uint sinceTimestamp) public returns(bool, uint) {
          if (condition(valueHistory_[0].value)) {
              return (true, sinceTimestamp);
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

      constructor(ObsWrapper wrapper) public BoolObservable(wrapper) {
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

beforeBoolS :: Time -> Int -> (T.Text, T.Text)
beforeBoolS time idx =
  ([text|BeforeBoolObservable_${idx'}|],
  [text|
  contract BeforeBoolObservable_${idx'} is BoolObservable {
      BoolObservable.Value[] valueHistory_;

      constructor(ObsWrapper wrapper) public BoolObservable(wrapper) {
      }

      function getValueHistory() public returns(BoolObservable.Value[]) {
          valueHistory_.length = 0;
          valueHistory_.push(BoolObservable.Value(true, 0));
          if (block.timestamp >= ${time'}) {
              valueHistory_.push(BoolObservable.Value(false, ${time'}));
          }
          return valueHistory_;
      }

      function getValue() public view returns(bool) {
          return block.timestamp < ${time'} ? true : false;
      }

      function getTimestamp() public view returns(uint) {
          return block.timestamp < ${time'} ? 0 : ${time'};
      }

      function getFirstSince(function(bool) external pure returns(bool) condition, uint since) public returns(bool, uint) {
          if (now >= since) {
              if (since < ${time'}) {
                  if (condition(true)) {
                      return (true, since);
                  } else if (now >= ${time'} && condition(false)) {
                      return (true, ${time'});
                  } else {
                      return (false, 0);
                  }
              } else {
                  if (condition(false)) {
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

atBoolS :: Time -> Int -> (T.Text, T.Text)
atBoolS time idx =
  ([text|AtBoolObservable_${idx'}|],
  [text|
  contract AtBoolObservable_${idx'} is BoolObservable {
      BoolObservable.Value[] valueHistory_;

      constructor(ObsWrapper wrapper) public BoolObservable(wrapper) {
      }

      function getValueHistory() public returns(BoolObservable.Value[]) {
          valueHistory_.length = 0;
          valueHistory_.push(BoolObservable.Value(false, 0));
          if (block.timestamp >= ${time'}) {
              valueHistory_.push(BoolObservable.Value(true, ${time'}));
          }
          if (block.timestamp > ${time'}) {
              valueHistory_.push(BoolObservable.Value(false, ${time'} + 1));
          }
          return valueHistory_;
      }

      function getValue() public view returns(bool) {
          return block.timestamp == ${time'} ? false : true;
      }

      function getTimestamp() public view returns(uint) {
          return block.timestamp == ${time'} ? ${time'} : (block.timestamp < ${time'} ? 0 : ${time'} + 1);
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

andBoolS :: Int -> Int -> Int -> (T.Text, T.Text)
andBoolS = binaryS "And" "&&" SBool SBool

greaterThanBoolS :: Int -> Int -> Int -> (T.Text, T.Text)
greaterThanBoolS = binaryS "GreaterThan" ">" SInt SBool

binaryS :: T.Text -> T.Text -> SType -> SType -> Int -> Int -> Int -> (T.Text, T.Text)
binaryS opName opSolidity inputType outputType obsIndex1 obsIndex2 idx =
  ([text|${opName}${outputType'}_${idx'}|],
  [text|
  contract ${opName}${outputType'}_${idx'} is ${outputType'} {
      ${inputType'} b1_;
      ${inputType'} b2_;
      ${outputType'}.Value[] public valueHistory_;

      constructor(ObsWrapper wrapper) public ${outputType'}(wrapper) {
          b1_ = wrapper_.deploy${inputType'}(${obsIndex1'});
          b2_ = wrapper_.deploy${inputType'}(${obsIndex2'});
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
                  valueHistory_.push(${outputType'}.Value(b1[i].value ${opSolidity} b2[j-1].value, b1[i].timestamp));
                  i++;
              } else if (b1[i].timestamp > b2[i].timestamp) {
                  valueHistory_.push(${outputType'}.Value(b1[i-1].value ${opSolidity} b2[j].value, b2[j].timestamp));
                  j++;
              } else {
                  valueHistory_.push(${outputType'}.Value(b1[i].value ${opSolidity} b2[i].value, b1[i].timestamp));
                  i++;
                  j++;
              }
          }
          while (i < b1.length) {
              valueHistory_.push(${outputType'}.Value(b1[i].value ${opSolidity} b2[j-1].value, b1[i].timestamp));
              i++;
          }
          while (j < b2.length) {
              valueHistory_.push(${outputType'}.Value(b1[i-1].value ${opSolidity} b2[j].value, b2[j].timestamp));
              j++;
          }

          return valueHistory_;
      }

      function getValue() public view returns(${outputRawType'}) {
          return b1_.getValue() ${opSolidity} b2_.getValue();
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
        obsIndex1' = showt obsIndex1
        obsIndex2' = showt obsIndex2

constantIntS :: Int -> Int -> (T.Text, T.Text)
constantIntS value idx =
  ([text|ConstantIntObservable_${idx'}|],
  [text|
  contract ConstantIntObservable_${idx'} is IntObservable {
      IntObservable.Value[] public valueHistory_;

      constructor(ObsWrapper wrapper) public IntObservable(wrapper) {
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

      function getFirstSince(function(int) external pure returns(bool) condition, uint sinceTimestamp) public returns(bool, uint) {
          if (condition(valueHistory_[0].value)) {
              return (true, sinceTimestamp);
          } else {
              return (false, 0);
          }
      }
  }
  |])
  where value' = showt value
        idx' = showt idx


subtractIntS :: Int -> Int -> Int -> (T.Text, T.Text)
subtractIntS = binaryS "Subtract" "-" SInt SInt

baseObservableS :: T.Text -> T.Text -> T.Text
baseObservableS name typeName =
  [text|
  contract ${name}Observable {
      struct Value {
          ${typeName} value;
          uint timestamp;
      }

      ObsWrapper wrapper_;

      constructor(ObsWrapper wrapper) public {
          wrapper_ = wrapper;
      }

      function getValueHistory() public returns(${name}Observable.Value[]);
      function getValue() public view returns(${typeName});
      function getTimestamp() public view returns(uint);

      function getFirstSince(function(${typeName}) external pure returns(bool) condition, uint sinceTimestamp) public returns(bool, uint);
  }
  |]

userObservableS :: T.Text -> T.Text -> T.Text
userObservableS name typeName =
  [text|
  contract User${name}Observable is ${name}Observable {
      ${name}Observable.Value[] private valueHistory_;
      address authority_;

      event ValueUpdated(${typeName} newValue);

      constructor(address authority, ${typeName} initialValue) public {
          valueHistory_.push(Value(initialValue, 0));
          authority_ = authority;
      }

      function getValueHistory() public returns (${name}Observable.Value[]) {
          return valueHistory_;
      }

      function getValue() public view returns (${typeName}) {
          return valueHistory_[valueHistory_.length - 1].value;
      }

      function getTimestamp() public view returns (uint) {
          return valueHistory_[valueHistory_.length - 1].timestamp;
      }

      function setValue(${typeName} value) public {
          valueHistory_.push(${name}Observable.Value(value, block.timestamp));
          emit ValueUpdated(value);
      }

      function getFirstSince(function(${typeName}) external pure returns (bool) condition, uint sinceTimestamp) public returns (bool, uint) {
          uint currentTimestamp = 0;
          ${typeName} currentValue = valueHistory_[0].value;
          bool meetsCondition = condition(currentValue);

          for (uint i = 0; i < valueHistory_.length; i++) {
              currentTimestamp = valueHistory_[i].timestamp;

              // if already met condition and next timestamp is beyond start of window
              if (currentTimestamp >= sinceTimestamp && meetsCondition) {
                  return (true, sinceTimestamp);
              }

              // if new timestamp is beyond now but did not already meet condition
              if (currentTimestamp > now) {
                  return (false, 0);
              }

              currentValue = valueHistory_[i].value;
              meetsCondition = condition(currentValue);

              // if new value meets condition and new timestamp is beyond start of window
              if (currentTimestamp >= sinceTimestamp && meetsCondition) {
                  return (true, currentTimestamp);
              }
          }

          if (now >= sinceTimestamp && meetsCondition) {
              return (true, sinceTimestamp);
          }

          return (false, 0);
      }
  }
  |]
