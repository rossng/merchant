{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module SolidityLibrary where

import NeatInterpolation (text)
import qualified Data.Text as T

import SolidityObservable

library :: T.Text
library = T.unlines [baseContract, marketplaceContract]

headers :: T.Text
headers = [text|
  pragma solidity ^0.4.23;
  pragma experimental ABIEncoderV2;
  |]

baseContract :: T.Text
baseContract = [text|
  contract BaseContract {
      enum KillReason {EXECUTED, UNTIL, HORIZON, FAILED}

      event Killed(BaseContract.KillReason killReason);

      Marketplace public marketplace_;
      int public scale_;

      address public creator_;
      bool public alive_ = true;

      constructor(Marketplace marketplace, int scale) public {
          marketplace_ = marketplace;
          scale_ = scale;
          creator_ = msg.sender;
      }

      function getHolder() internal view returns(address) {
          address holder;
          (,holder,,) = marketplace_.contracts_(this);
          return holder;
      }

      function getCounterparty() internal view returns(address) {
          address counterparty;
          (counterparty,,,) = marketplace_.contracts_(this);
          return counterparty;
      }

      function getCreator() internal view returns(address) {
          address creator;
          (,,creator,) = marketplace_.contracts_(this);
          return creator;
      }

      function proceed() public;

      function receive(Marketplace.Commodity commodity, int quantity) internal whenAlive {
          marketplace_.receive(commodity, quantity);
      }

      function kill(BaseContract.KillReason killReason) internal whenAlive {
          alive_ = false;
          emit Killed(killReason);
      }

      modifier whenAlive {
          require(alive_);
          _;
      }
  }
|]

marketplaceContract :: T.Text
marketplaceContract = [text|
  contract Marketplace {
      enum Commodity {USD, GBP}

      struct ContractMetadata {
          address counterparty;
          address holder;
          address creator;
          bool signed;
      }

      event Proposed(address contractAddress, address indexed to);
      event Signed(address contractAddress);
      event Delegated(address indexed from, address to);

      address public creator_;

      mapping(address => ContractMetadata) public contracts_;

      mapping(address => mapping(uint => int)) public balances_;

      constructor() public {
          balances_[msg.sender][uint(Commodity.USD)] = 0;
          balances_[msg.sender][uint(Commodity.GBP)] = 0;
          creator_ = msg.sender;
      }

      function signed(address contractAddress) public constant returns (bool) {
          return contracts_[contractAddress].signed;
      }

      function propose(BaseContract contractAddress, address to) public {
          require(contractAddress.creator_() == msg.sender);
          // is being requested by creator of contract
          require(!contracts_[contractAddress].signed);
          // has not already been signed
          contracts_[contractAddress] = ContractMetadata(msg.sender, to, msg.sender, false);
          emit Proposed(contractAddress, to);
      }

      function sign(address contractAddress) public {
          require(msg.sender == contracts_[contractAddress].holder);
          require(!contracts_[contractAddress].signed);
          contracts_[contractAddress].signed = true;
          BaseContract baseContract = BaseContract(contractAddress);
          baseContract.proceed();
          emit Signed(contractAddress);
      }

      function receive(Commodity commodity, int quantity) public {
          ContractMetadata storage c = contracts_[msg.sender];
          require(c.signed == true);
          balances_[c.counterparty][uint(commodity)] -= quantity;
          balances_[c.holder][uint(commodity)] += quantity;
      }

      function delegate(address newContract) public {
          require(contracts_[msg.sender].signed == true);
          contracts_[newContract] = ContractMetadata(
              contracts_[msg.sender].counterparty,
              contracts_[msg.sender].holder,
              msg.sender,
              true
          );
          emit Delegated(msg.sender, newContract);
      }

      function give(address newContract) public {
          require(contracts_[msg.sender].signed == true);
          contracts_[newContract] = ContractMetadata(
              contracts_[msg.sender].holder,
              contracts_[msg.sender].counterparty,
              msg.sender,
              true
          );
          emit Delegated(msg.sender, newContract);
      }

      function transfer(address to, Commodity commodity, uint quantity) public {
          require(uint(balances_[msg.sender][uint(commodity)]) >= quantity);
          balances_[msg.sender][uint(commodity)] -= int(quantity);
          balances_[to][uint(commodity)] += int(quantity);
      }

      function award(address to, Commodity commodity, uint quantity) public {
          require(msg.sender == creator_);
          balances_[to][uint(commodity)] += int(quantity);
      }

      function isTrue(bool input) external pure returns(bool) {
          return input;
      }

      function isFalse(bool input) external pure returns(bool) {
          return !input;
      }
  }
|]

observableLibrary :: T.Text
observableLibrary = T.unlines [boolObservable, intObservable, userBoolObservable, userIntObservable]
  where boolObservable = baseObservableS "Bool" "bool"
        intObservable = baseObservableS "Int" "int"
        userBoolObservable = userObservableS "Bool" "bool"
        userIntObservable = userObservableS "Int" "int"

fullLibrary :: T.Text
fullLibrary = T.unlines [headers, baseContract, marketplaceContract, boolObservable, intObservable, userBoolObservable, userIntObservable]
  where boolObservable = baseObservableS "Bool" "bool"
        intObservable = baseObservableS "Int" "int"
        userBoolObservable = userObservableS "Bool" "bool"
        userIntObservable = userObservableS "Int" "int"