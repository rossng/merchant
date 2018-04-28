{-# LANGUAGE QuasiQuotes #-}
module SolidityLibrary where

import NeatInterpolation (text)
import qualified Data.Text as T

baseContract :: T.Text
baseContract =
  [text|
  contract BaseContract {
      event Killed();

      Marketplace public marketplace_;
      int public scale_;

      address public creator_;
      bool public alive_ = true;

      constructor(Marketplace marketplace, int scale) public {
          marketplace_ = marketplace;
          scale_ = scale;
          creator_ = msg.sender;
      }

      function getHolder() internal returns(address) {
          return marketplace.contracts_[this].holder;
      }

      function proceed() public;

      function receive(Marketplace.Commodity commodity, int quantity) internal whenAlive {
          marketplace_.receive(commodity, quantity);
      }

      function kill() internal whenAlive {
          alive_ = false;
          emit Killed();
      }

      modifier whenAlive {
          require(alive_);
          _;
      }
  }
  |]