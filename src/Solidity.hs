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

data SType = SInt | SBool
data Solidity = Solidity {
  _source :: [T.Text],
  _counter :: Int,
  _runtimeObservables :: [SType]
}
makeLenses ''Solidity

initialSolidity :: Solidity
initialSolidity = Solidity { _source = [], _counter = 0, _runtimeObservables = [] }

data SObservableAddress = PredefinedAddress String | RequiresAddress | Unaddressed
data SObservable = SObservable SType SObservableAddress

class SolidityObservable a where
  toSolidityObservable :: Obs a -> SObservable
  toSolidityLiteral :: Obs a -> T.Text

instance SolidityObservable Int where
  toSolidityObservable (External addr) = SObservable SInt (PredefinedAddress addr)
  toSolidityObservable (Constant n) = SObservable SInt Unaddressed
  toSolidityLiteral (External addr) = [text|ObservableInt(${addr'}).getValue()|]
    where addr' = showt addr
  toSolidityLiteral (Constant n) = showt n

instance SolidityObservable Bool where
  toSolidityObservable (External addr) = SObservable SBool (PredefinedAddress addr)
  toSolidityObservable (Constant b) = SObservable SBool Unaddressed
  toSolidityObservable (After t) = SObservable SBool Unaddressed
  toSolidityObservable (OAnd b1 b2) = SObservable SBool Unaddressed
  toSolidityLiteral (External addr) = [text|ObservableBool(${addr'}).getValue()|]
    where addr' = showt addr
  toSolidityLiteral (Constant True) = "true"
  toSolidityLiteral (Constant False) = "false"
  toSolidityLiteral (After t) = [text|(now > ${t'})|]
    where t' = showt t
  toSolidityLiteral (OAnd b1 b2) = [text|${b1'} && ${b2'}|]
    where b1' = toSolidityLiteral b1
          b2' = toSolidityLiteral b2

class Functor f => SolidityAlg f where
  solidityAlg :: f (State Solidity (T.Text, Horizon)) -> State Solidity (T.Text, Horizon)

addClass cls sources = cls : sources

instance SolidityAlg ContractF where
  solidityAlg Zero = do
    let horizon = Infinite
    counter %= (+1)
    n <- use counter
    source %= (addClass $ zeroS horizon (showt n))
    return ("Zero_" `T.append` showt n, horizon)
  solidityAlg (One k) = do
    let horizon = Infinite
    counter %= (+1)
    n <- use counter
    source %= (addClass $ oneS horizon k (showt n))
    return ("One_" `T.append` showt n, horizon)
  solidityAlg (Give c) = do
    counter %= (+1)
    n <- use counter
    (className, horizon) <- c
    source %= (addClass $ giveS horizon className (showt n))
    return ("Give_" `T.append` showt n, horizon)
  solidityAlg (And c1 c2) = do
    (className1, horizon1) <- c1
    (className2, horizon2) <- c2
    let horizon = max horizon1 horizon2
    counter %= (+1)
    n <- use counter
    source %= (addClass $ andS horizon className1 className2 (showt n))
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
    source %= (addClass $ orS horizon className1 className2 observableLiteral (showt n))
    return ("Or_" `T.append` showt n, horizon)
  solidityAlg (Scale obs c) = do
    (className, horizon) <- c
    counter %= (+1)
    n <- use counter
    source %= (addClass $ scaleS horizon className (toSolidityLiteral obs) (showt n))
    return ("Scale_" `T.append` showt n, horizon)

instance SolidityAlg OriginalF where
  solidityAlg (Truncate t c) = do
    (className, horizon1) <- c
    let horizon = min horizon1 (Time t)
    counter %= (+1)
    n <- use counter
    source %= (addClass $ truncateS horizon className (showt t) (showt n))
    return ("Truncate_" `T.append` showt n, horizon)
  solidityAlg (Then c1 c2) = do
    (className1, horizon1) <- c1
    (className2, horizon2) <- c2
    let horizon = max horizon1 horizon2
    counter %= (+1)
    n <- use counter
    source %= (addClass $ thenS horizon className1 className2 horizon1 horizon2 (showt n))
    return ("Then_" `T.append` showt n, horizon)
  solidityAlg (Get c) = do
    (className, horizon) <- c
    counter %= (+1)
    n <- use counter
    source %= (addClass $ getS horizon className (showt n))
    return ("Get_" `T.append` showt n, horizon)
  solidityAlg (Anytime c) = do
    (className, horizon) <- c
    counter %= (+1)
    n <- use counter
    source %= (addClass $ anytimeS horizon className (showt n))
    return ("Anytime_" `T.append` showt n, horizon)

instance SolidityAlg ExtendedF where
  solidityAlg = undefined


instance (SolidityAlg f, SolidityAlg g) => SolidityAlg (f :+ g) where
  solidityAlg (L x) = solidityAlg x
  solidityAlg (R y) = solidityAlg y

makeClass :: Horizon -> T.Text -> T.Text -> T.Text
makeClass horizon className proceed =
  [text|
  contract ${className} is BaseContract {
      WrapperContract public wrapper_;
      function ${className}(Marketplace marketplace, int scale, WrapperContract wrapper) public BaseContract(marketplace, scale) {
          wrapper_ = wrapper;
      }

      function proceed() public whenAlive {
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
zeroS horizon n = makeClass horizon [text|Zero_${n}|] "kill();"

oneS :: Horizon -> Currency -> T.Text -> T.Text
oneS horizon k n = makeClass horizon
  [text|One_${n}|]
  [text|
  marketplace_.receive(Marketplace.Commodity.${k'}, 1);
  kill();|]
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
  ${className} next = new ${className}(marketplace_, scale_, wrapper_);
  marketplace_.give(next);
  kill();
  |]

andS :: Horizon -> T.Text -> T.Text -> T.Text -> T.Text
andS horizon className1 className2 n = makeClass horizon
  [text|And_${n}|]
  [text|
  ${className1} next1 = new ${className1}(marketplace_, scale_, wrapper_);
  ${className2} next2 = new ${className2}(marketplace_, scale_, wrapper_);
  marketplace_.delegate(next1);
  marketplace_.delegate(next2);
  next1.proceed();
  next2.proceed();
  kill();
  |]

orS :: Horizon -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text
orS horizon className1 className2 obs n = makeClass horizon
  [text|Or_${n}|]
  [text|
  if (${obs}) {
      ${className2} next2 = new ${className2}(marketplace_, scale_, wrapper_);
      marketplace_.delegate(next2);
      next2.proceed();
  } else {
      ${className1} next1 = new ${className1}(marketplace_, scale_, wrapper_);
      marketplace_.delegate(next1);
      next1.proceed();
  }
  kill();
  |]

scaleS :: Horizon -> T.Text -> T.Text -> T.Text -> T.Text
scaleS horizon className factor n = makeClass horizon
  [text|Scale_${n}|]
  [text|
  ${className} next = new ${className}(marketplace_, scale_ * ${factor}, wrapper_);
  marketplace_.delegate(next);
  next.proceed();
  kill();
  |]

truncateS :: Horizon -> T.Text -> T.Text -> T.Text -> T.Text
truncateS horizon className time n = makeClass horizon
  [text|Truncate_${n}|]
  [text|
  if (now <= ${time}) {
      ${className} next = new ${className}(marketplace_, scale_, wrapper_);
      marketplace_.delegate(next);
      next.proceed();
  }
  kill();
  |]

thenS :: Horizon -> T.Text -> T.Text -> Horizon -> Horizon -> T.Text -> T.Text
thenS horizon className1 className2 horizon1 horizon2 n = makeClass horizon
  [text|Then_${n}|]
  [text|
  if (${horizon1'}) {
      ${className1} next1 = new ${className1}(marketplace_, scale_, wrapper_);
      marketplace_.delegate(next1);
      next1.proceed();
  } else if (${horizon2'}) {
      ${className2} next2 = new ${className2}(marketplace_, scale_, wrapper_);
      marketplace_.delegate(next2);
      next2.proceed();
  }
  kill();
  |]
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
      ${className} next = new ${className}(marketplace_, scale_, wrapper_);
      marketplace_.delegate(next);
      next.proceed();
      kill();
  }
  |]
  where
    horizon' = case horizon of
      Time t -> let t' = showt t in [text|now == ${t'}|]
      Infinite -> "false"

anytimeS :: Horizon -> T.Text -> T.Text -> T.Text
anytimeS horizon className n =
  [text|
  contract Anytime_${n} is BaseContract {
      bool public ready_;
      WrapperContract public wrapper_;
      function Anytime_${n}(Marketplace marketplace, int scale, WrapperContract wrapper) public BaseContract(marketplace, scale) {
          wrapper_ = wrapper;
      }

      function proceed() public whenAlive {
          if (${horizonCheck}) {
              kill();
              return;
          }
          if (!ready_) {
              ready_ = true;
          } else {
              ${className} next = new ${className}(marketplace_, scale_, wrapper_);
              marketplace_.delegate(next);
              next.proceed();
              kill();
          }
      }
  }
  |]
  where
    horizonCheck = case horizon of
      Time t -> let t' = showt t in [text|now > ${t'}|]
      Infinite -> [text|false|]

wrapper :: [SType] -> T.Text -> T.Text
wrapper observableTypes rootClass =
  [text|
  contract WrapperContract is BaseContract {
      ${observableMembers}

      function WrapperContract(Marketplace marketplace, int scale${observableParameters}) public BaseContract(marketplace, scale) {
      }

      function proceed() public whenAlive {
          ${rootClass} next = new ${rootClass}(marketplace_, 1, this);
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
                             SInt -> [text|, ObservableInt obs${i'}|]
                             SBool -> [text|, ObservableBool obs${i'}|]) observableTypes
    observableMembers :: T.Text
    observableMembers = T.unlines $ IList.imap (\i t -> let i' = showt i in case t of
                             SInt -> [text|ObservableInt obs${i'}_;|]
                             SBool -> [text|ObservableBool obs${i'}_;|]) observableTypes


compileContract :: Contract -> T.Text
compileContract (Pure _) = ""
compileContract c = T.concat $ (wrapper (solidity ^. runtimeObservables) rootClass) : (solidity ^. source)
  where
    compileState = handle (const $ return ("", Infinite)) solidityAlg c
    ((rootClass, horizon), solidity) = runState compileState initialSolidity