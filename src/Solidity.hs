{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Solidity where

import Declarative

import NeatInterpolation (text)
import qualified Data.Text as T

data Solidity = Solidity

class Functor f => SolidityAlg f where
  ethereumAlg :: f Solidity -> Solidity

instance SolidityAlg ContractF where
  ethereumAlg Zero = Solidity

zeroS :: T.Text
zeroS =
  [text|
contract Zero is BaseContract {
    function Zero(Marketplace marketplace) BaseContract(marketplace) public {
        kill();
    }
}
  |]