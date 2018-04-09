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
  solidityAlg :: f (State Solidity T.Text) -> State Solidity T.Text

addClass cls sources = cls : sources

instance SolidityAlg ContractF where
  solidityAlg Zero = do
    counter %= (+1)
    n <- use counter
    source %= (addClass $ zeroS (showt n))
    return ("Zero_" `T.append` showt n)
  solidityAlg (One k) = do
    counter %= (+1)
    n <- use counter
    source %= (addClass $ oneS k (showt n))
    return ("One_" `T.append` showt n)
  solidityAlg (Give c) = do
    counter %= (+1)
    n <- use counter
    className <- c
    source %= (addClass $ giveS className (showt n))
    return ("Give_" `T.append` showt n)
  solidityAlg (And c1 c2) = do
    className1 <- c1
    className2 <- c2
    counter %= (+1)
    n <- use counter
    source %= (addClass $ andS className1 className2 (showt n))
    return ("And_" `T.append` showt n)
  solidityAlg (Or c1 c2) = do
    className1 <- c1
    className2 <- c2
    counter %= (+1)
    n <- use counter
    o <- showt . length <$> use runtimeObservables
    runtimeObservables %= (++ [SBool])
    let observableLiteral = [text|wrapper_.obs${o}_.getValue()|]
    source %= (addClass $ orS className1 className2 observableLiteral (showt n))
    return ("Or_" `T.append` showt n)
  solidityAlg (Scale obs c) = do
    className <- c
    counter %= (+1)
    n <- use counter
    source %= (addClass $ scaleS className (toSolidityLiteral obs) (showt n))
    return ("Scale_" `T.append` showt n)

instance SolidityAlg OriginalF where
  solidityAlg = undefined

instance SolidityAlg ExtendedF where
  solidityAlg = undefined


instance (SolidityAlg f, SolidityAlg g) => SolidityAlg (f :+ g) where
  solidityAlg (L x) = solidityAlg x
  solidityAlg (R y) = solidityAlg y

makeClass :: T.Text -> T.Text -> T.Text
makeClass className proceed =
  [text|
  contract ${className} is BaseContract {
      WrapperContract public wrapper_;
      function ${className}(Marketplace marketplace, int scale, WrapperContract wrapper) public BaseContract(marketplace, scale) {
          wrapper_ = wrapper;
      }

      function proceed() public whenAlive {
          ${proceed}
      }
  }
  |]

zeroS :: T.Text -> T.Text
zeroS n = makeClass [text|Zero_${n}|] "kill();"

oneS :: Currency -> T.Text -> T.Text
oneS k n = makeClass
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

giveS :: T.Text -> T.Text -> T.Text
giveS className n = makeClass
  [text|Give_${n}|]
  [text|
  ${className} next = new ${className}(marketplace_, scale_, wrapper_);
  marketplace_.give(next);
  kill();
  |]

andS :: T.Text -> T.Text -> T.Text -> T.Text
andS className1 className2 n = makeClass
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

orS :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text
orS className1 className2 obs n = makeClass
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

scaleS :: T.Text -> T.Text -> T.Text -> T.Text
scaleS className factor n = makeClass
  [text|Scale_${n}|]
  [text|
  ${className} next = new ${className}(marketplace_, scale_ * ${factor}, wrapper_);
  marketplace_.delegate(next);
  next.proceed();
  kill();
  |]

--truncateS :: T.Text -> T.Text -> T.Text
--truncateS className time = makeClass
--  [text|Truncate_${n}|]
--  [text|
--  if (time
--  |]

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
    compileState = handle (const $ return "") solidityAlg c
    (rootClass, solidity) = runState compileState initialSolidity