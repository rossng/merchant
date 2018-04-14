{-# LANGUAGE QuasiQuotes, OverloadedStrings, TypeOperators, TemplateHaskell, GADTs #-}
module Solidity where

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
import SolidityObservable

data Solidity = Solidity {
  _source :: [T.Text],
  _counter :: Int,
  _runtimeObservables :: [SType],
  _observableState :: ObsCompileState
}
makeLenses ''Solidity

initialSolidity :: Solidity
initialSolidity = Solidity {
  _source = [],
  _counter = 0,
  _runtimeObservables = [],
  _observableState = initialCompileState
}

obsSolidityAlg :: Solidifiable a => Obs a -> State ObsCompileState T.Text
obsSolidityAlg obs = compileSolidityObs graph (graph !! rootIdx)
  where (rootIdx, graph) = runState (obsToGraph obs) []

class Functor f => SolidityAlg f where
  solidityAlg :: f (State Solidity (T.Text, Horizon)) -> State Solidity (T.Text, Horizon)

instance SolidityAlg ContractF where
  solidityAlg Zero = do
    let horizon = Infinite
    counter %= (+1)
    n <- use counter
    source %= addClass (zeroS horizon (showt n))
    return ("Zero_" `T.append` showt n, horizon)
  solidityAlg (One k) = do
    let horizon = Infinite
    counter %= (+1)
    n <- use counter
    source %= addClass (oneS horizon k (showt n))
    return ("One_" `T.append` showt n, horizon)
  solidityAlg (Give c) = do
    counter %= (+1)
    n <- use counter
    (className, horizon) <- c
    source %= addClass (giveS horizon className (showt n))
    return ("Give_" `T.append` showt n, horizon)
  solidityAlg (And c1 c2) = do
    (className1, horizon1) <- c1
    (className2, horizon2) <- c2
    let horizon = max horizon1 horizon2
    counter %= (+1)
    n <- use counter
    source %= addClass (andS horizon className1 className2 (showt n))
    return ("And_" `T.append` showt n, horizon)
  solidityAlg (Or c1 c2) = do
    (className1, horizon1) <- c1
    (className2, horizon2) <- c2
    let horizon = max horizon1 horizon2
    counter %= (+1)
    n <- use counter
    o <- showt . length <$> use runtimeObservables
    runtimeObservables %= (++ [SBool])
    let observableLiteral = [text|wrapper_.obs${o}_.getValue()|]
    source %= addClass (orS horizon className1 className2 observableLiteral (showt n))
    return ("Or_" `T.append` showt n, horizon)
  solidityAlg (Scale obs c) = do
    (className, horizon) <- c
    counter %= (+1)
    n <- use counter
    obs <- zoom observableState (obsSolidityAlg obs)
    source %= addClass (scaleS horizon className obs (showt n))
    return ("Scale_" `T.append` showt n, horizon)

instance SolidityAlg OriginalF where
  solidityAlg (Truncate t c) = do
    (className, horizon1) <- c
    let horizon = min horizon1 (Time t)
    counter += 1
    n <- use counter
    source %= addClass (truncateS horizon className (showt t) (showt n))
    return ("Truncate_" `T.append` showt n, horizon)
  solidityAlg (Then c1 c2) = do
    (className1, horizon1) <- c1
    (className2, horizon2) <- c2
    let horizon = max horizon1 horizon2
    counter += 1
    n <- use counter
    source %= addClass (thenS horizon className1 className2 horizon1 horizon2 (showt n))
    return ("Then_" `T.append` showt n, horizon)
  solidityAlg (Get c) = do
    (className, horizon) <- c
    counter += 1
    n <- use counter
    source %= addClass (getS horizon className (showt n))
    return ("Get_" `T.append` showt n, horizon)
  solidityAlg (Anytime c) = do
    (className, horizon) <- c
    counter += 1
    n <- use counter
    source %= addClass (anytimeS horizon className (showt n))
    return ("Anytime_" `T.append` showt n, horizon)

instance SolidityAlg ExtendedF where
  solidityAlg (Cond obs c1 c2) = do
    (className1, horizon1) <- c1
    (className2, horizon2) <- c2
    let horizon = max horizon1 horizon2
    obsConstructor <- zoom observableState (obsSolidityAlg obs)
    counter += 1
    n <- use counter
    source %= addClass (condS horizon className1 className2 obsConstructor (showt n))
    return ("Cond_" `T.append` showt n, horizon)
  solidityAlg (When obs c) = do
    (className, horizon) <- c
    obsConstructor <- zoom observableState (obsSolidityAlg obs)
    counter += 1
    n <- use counter
    source %= addClass (whenS horizon className obsConstructor (showt n))
    return ("When_" `T.append` showt n, horizon)
  solidityAlg (AnytimeO obs c) = do
    (className, horizon) <- c
    obsConstructor <- zoom observableState (obsSolidityAlg obs)
    counter += 1
    n <- use counter
    source %= addClass (anytimeObsS horizon className obsConstructor (showt n))
    return ("AnytimeO_" `T.append` showt n, horizon)
  solidityAlg (Until obs c) = do
    (className, horizon) <- c
    obsConstructor <- zoom observableState (obsSolidityAlg obs)
    counter += 1
    n <- use counter
    source %= addClass (untilS horizon className obsConstructor (showt n))
    return ("Until_" `T.append` showt n, horizon)


instance (SolidityAlg f, SolidityAlg g) => SolidityAlg (f :+ g) where
  solidityAlg (L x) = solidityAlg x
  solidityAlg (R y) = solidityAlg y

makeClass :: Horizon -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text
makeClass horizon className proceed members constructor =
  [text|
  contract ${className} is BaseContract {
      WrapperContract public wrapper_;
      bool public until_;
      BoolObservable private untilObs_;
      uint acquiredTimestamp_;
      ${members}
      constructor(Marketplace marketplace, int scale, WrapperContract wrapper, bool until, BoolObservable untilObs) public BaseContract(marketplace, scale) {
          wrapper_ = wrapper;
          until_ = until;
          untilObs_ = untilObs;
          acquiredTimestamp_ = block.timestamp;
          ${constructor}
      }

      function proceed() public whenAlive {
          if (until_) {
              bool untilFulfilled;
              (untilFulfilled,) = untilObs_.getFirstSince(marketplace_.isTrue, acquiredTimestamp_);
              if (untilFulfilled) {
                  kill();
                  return;
              }
          }
          ${horizonCheck}
          ${proceed}
      }
  }
  |]
  where
    horizonCheck = case horizon of
      Time t -> let t' = showt t in [text|
        if(now > ${t'}) {
            kill();
            return;
        }
      |]
      Infinite -> ""

zeroS :: Horizon -> T.Text -> T.Text
zeroS horizon n = makeClass horizon [text|Zero_${n}|] "kill();" "" ""

oneS :: Horizon -> Currency -> T.Text -> T.Text
oneS horizon k n = makeClass horizon
  [text|One_${n}|]
  [text|
  marketplace_.receive(Marketplace.Commodity.${k'}, scale_);
  kill();|]
  ""
  ""
  where
    currency :: Currency -> T.Text
    currency GBP = "GBP"
    currency USD = "USD"
    currency EUR = "EUR"
    k' = currency k

giveS :: Horizon -> T.Text -> T.Text -> T.Text
giveS horizon className n = makeClass horizon
  [text|Give_${n}|]
  [text|
  ${className} next = new ${className}(marketplace_, scale_, wrapper_, false, BoolObservable(0));
  marketplace_.give(next);
  kill();
  |]
  ""
  ""

andS :: Horizon -> T.Text -> T.Text -> T.Text -> T.Text
andS horizon className1 className2 n = makeClass horizon
  [text|And_${n}|]
  [text|
  ${className1} next1 = new ${className1}(marketplace_, scale_, wrapper_, false, BoolObservable(0));
  ${className2} next2 = new ${className2}(marketplace_, scale_, wrapper_, false, BoolObservable(0));
  marketplace_.delegate(next1);
  marketplace_.delegate(next2);
  next1.proceed();
  next2.proceed();
  kill();
  |]
  ""
  ""

orS :: Horizon -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text
orS horizon className1 className2 obs n = makeClass horizon
  [text|Or_${n}|]
  [text|
  if (${obs}) {
      ${className2} next2 = new ${className2}(marketplace_, scale_, wrapper_, false, BoolObservable(0));
      marketplace_.delegate(next2);
      next2.proceed();
  } else {
      ${className1} next1 = new ${className1}(marketplace_, scale_, wrapper_, false, BoolObservable(0));
      marketplace_.delegate(next1);
      next1.proceed();
  }
  kill();
  |]
  ""
  ""

scaleS :: Horizon -> T.Text -> T.Text -> T.Text -> T.Text
scaleS horizon className obsConstructor n = makeClass horizon
  [text|Scale_${n}|]
  [text|
  ${className} next = new ${className}(marketplace_, scale_ * obs_.getValue(), wrapper_, false, BoolObservable(0));
  marketplace_.delegate(next);
  next.proceed();
  kill();
  |]
  "IntObservable private obs_;"
  [text|obs_ = ${obsConstructor};|]

truncateS :: Horizon -> T.Text -> T.Text -> T.Text -> T.Text
truncateS horizon className time n = makeClass horizon
  [text|Truncate_${n}|]
  [text|
  if (now <= ${time}) {
      ${className} next = new ${className}(marketplace_, scale_, wrapper_, false, BoolObservable(0));
      marketplace_.delegate(next);
      next.proceed();
  }
  kill();
  |]
  ""
  ""

thenS :: Horizon -> T.Text -> T.Text -> Horizon -> Horizon -> T.Text -> T.Text
thenS horizon className1 className2 horizon1 horizon2 n = makeClass horizon
  [text|Then_${n}|]
  [text|
  if (${horizon1'}) {
      ${className1} next1 = new ${className1}(marketplace_, scale_, wrapper_, false, BoolObservable(0));
      marketplace_.delegate(next1);
      next1.proceed();
  } else if (${horizon2'}) {
      ${className2} next2 = new ${className2}(marketplace_, scale_, wrapper_, false, BoolObservable(0));
      marketplace_.delegate(next2);
      next2.proceed();
  }
  kill();
  |]
  ""
  ""
  where
    horizon1' = case horizon1 of
      Time t -> let t' = showt t in [text|now <= ${t'}|]
      Infinite -> "true"
    horizon2' = case horizon2 of
      Time t -> let t' = showt t in [text|now <= ${t'}|]
      Infinite -> "false"


getS :: Horizon -> T.Text -> T.Text -> T.Text
getS horizon className n = makeClass horizon
  [text|Get_${n}|]
  [text|
  if (${horizon'}) {
      ${className} next = new ${className}(marketplace_, scale_, wrapper_, false, BoolObservable(0));
      marketplace_.delegate(next);
      next.proceed();
      kill();
  }
  |]
  ""
  ""
  where
    horizon' = case horizon of
      Time t -> let t' = showt t in [text|now == ${t'}|]
      Infinite -> "false"

anytimeS :: Horizon -> T.Text -> T.Text -> T.Text
anytimeS horizon className n = makeClass horizon
  [text|Anytime_${n}|]
  [text|
  if (!ready_) {
      ready_ = true;
  } else if (msg.sender == marketplace_.contracts_[this].holder) {
      ${className} next = new ${className}(marketplace_, scale_, wrapper_, false, BoolObservable(0));
      marketplace_.delegate(next);
      next.proceed();
      kill();
  }
  |]
  "bool public ready_ = false;"
  ""
  where
    horizonCheck = case horizon of
      Time t -> let t' = showt t in [text|now > ${t'}|]
      Infinite -> [text|false|]

condS :: Horizon -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text
condS horizon className1 className2 obsConstructor n = makeClass horizon
  [text|Cond_${n}|]
  [text|
  if (obs_.getValue()) {
      ${className1} next1 = new ${className1}(marketplace_, scale_, wrapper_, false, BoolObservable(0));
      marketplace_.delegate(next1);
      next1.proceed();
  } else {
      ${className2} next2 = new ${className2}(marketplace_, scale_, wrapper_, false, BoolObservable(0));
      marketplace_.delegate(next2);
      next2.proceed();
  }
  kill();
  |]
  "BoolObservable private obs_;"
  [text|obs_ = ${obsConstructor};|]

whenS :: Horizon -> T.Text -> T.Text -> T.Text -> T.Text
whenS horizon className obsConstructor n = makeClass horizon
  [text|When_${n}|]
  [text|
  bool fulfilled;
  uint when;
  (fulfilled, when) = obs_.getFirstSince(this.isTrue, acquiredTimestamp_);
  if (fulfilled) {
      if (when == now) {
          ${className} next = new ${className}(marketplace_, scale_, wrapper_, false, BoolObservable(0));
          marketplace_.delegate(next);
          next.proceed();
          kill();
      } else if (when < now) {
          kill();
      }
  }
  |]
  [text|
  BoolObservable private obs_;

  function isTrue(bool input) external pure returns(bool) {
      return input;
  }
  |]
  [text|
  obs_ = ${obsConstructor};
  |]

anytimeObsS :: Horizon -> T.Text -> T.Text -> T.Text -> T.Text
anytimeObsS horizon className obsConstructor n = makeClass horizon
  [text|AnytimeO_${n}|]
  [text|
  if (obs_.getValue()) {
      ${className} next = new ${className}(marketplace_, scale_, wrapper_, false, BoolObservable(0));
      marketplace_.delegate(next);
      next.proceed();
      kill();
  }
  |]
  [text|
  BoolObservable private obs_;
  |]
  [text|
  obs_ = ${obsConstructor};
  |]

untilS :: Horizon -> T.Text -> T.Text -> T.Text -> T.Text
untilS horizon className obsConstructor n = makeClass horizon
  [text|Until_${n}|]
  [text|
  ${className} next = new ${className}(marketplace_, scale_, wrapper_, true, obs_);
  marketplace_.delegate(next);
  next.proceed();
  kill();
  |]
  [text|
  BoolObservable private obs_;
  |]
  [text|
  obs_ = ${obsConstructor};
  |]

wrapper :: [SType] -> T.Text -> T.Text
wrapper observableTypes rootClass =
  [text|
  contract WrapperContract is BaseContract {
      ${observableMembers}

      constructor(Marketplace marketplace${observableParameters}) public BaseContract(marketplace, 1) {
      }

      function proceed() public whenAlive {
          ${rootClass} next = new ${rootClass}(marketplace_, 1, this, false, BoolObservable(0));
          marketplace_.delegate(next);
          next.proceed();
          kill();
      }
  }
  |]
  where
  --TODO
    observableParameters :: T.Text
    observableParameters = T.concat $ IList.imap (\i t -> let i' = showt i in case t of
                             SInt -> [text|, IntObservable obs${i'}|]
                             SBool -> [text|, BoolObservable obs${i'}|]) observableTypes
    observableMembers :: T.Text
    observableMembers = T.unlines $ IList.imap (\i t -> let i' = showt i in case t of
                             SInt -> [text|IntObservable obs${i'}_;|]
                             SBool -> [text|BoolObservable obs${i'}_;|]) observableTypes


compileContract :: Contract -> T.Text
compileContract (Pure _) = ""
compileContract c = T.unlines $ declarations ++ contractSources ++ wrapperSource ++ baseObsSources ++ obsSources
  where
    compileState = handle (const $ return ("", Infinite)) solidityAlg c
    ((rootClass, horizon), solidity) = runState compileState initialSolidity
    declarations = [[text|
    pragma solidity ^0.4.21;
    pragma experimental ABIEncoderV2;
    import {BaseContract, Marketplace} from './Marketplace.sol';
    |]]
    contractSources = solidity ^. source
    wrapperSource = [wrapper (solidity ^. runtimeObservables) rootClass]
    baseObsSources = [baseObservableS "Int" "int", baseObservableS "Bool" "bool"]
    obsSources = solidity ^. observableState . obsSource