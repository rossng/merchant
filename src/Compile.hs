{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Compile where

import qualified Data.Text as T
import NeatInterpolation (text)
import System.IO (writeFile, hPutStr, hClose, readFile)
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)
import System.Process (callProcess)
import System.FilePath ((</>))
import TextShow

import Control.Monad.IO.Class (liftIO)
import DynFlags
import GHC
import GHC.Paths (libdir)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import Unsafe.Coerce (unsafeCoerce)

import Declarative
import Render
import qualified Solidity
import qualified SolidityLibrary

-- many thanks to eugenk - https://stackoverflow.com/questions/47680575/haskell-ghc-8-dynamically-load-import-module

pluginModuleNameStr :: String
pluginModuleNameStr = "TheContract"

makeContractModule :: T.Text -> String
makeContractModule contract = T.unpack [text|
  module TheContract where
  import Declarative
  import Observable
  contract :: Contract
  contract = ${contract}
|]

writeTempFile :: String -> IO FilePath
writeTempFile contract = do
  dir <- getTemporaryDirectory
  let file = dir ++ "/" ++ pluginModuleNameStr ++ ".hs"
  writeFile file (makeContractModule (T.pack contract))
  return file

interpretHaskell :: String -> IO Contract
interpretHaskell contract = do
  moduleFile <- writeTempFile contract
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      _ <- setSessionDynFlags dflags
      target <- guessTarget moduleFile Nothing
      setTargets [target]
      r <- load LoadAllTargets
      liftIO $ removePathForcibly moduleFile
      case r of
        Failed -> error "Compilation failed"
        Succeeded -> do
          setContext [IIDecl $ simpleImportDecl $ mkModuleName pluginModuleNameStr]
          result <- compileExpr "TheContract.contract"
          let result' = unsafeCoerce result :: Contract
          return result'

runSolc :: T.Text -> (FilePath -> IO a) -> IO a
runSolc solidityCode callback =
  withSystemTempFile "solidity.sol" $ \filePath handle -> do
    hPutStr handle (T.unpack solidityCode)
    hClose handle
    withSystemTempDirectory "solcout" $ \folderPath -> do
      callProcess "solc" ["--overwrite", "-o", folderPath, "--optimize", "--bin", "--abi", filePath]
      callback folderPath

getSolcOutput :: T.Text -> String -> IO T.Text
getSolcOutput solidityCode fileName = runSolc solidityCode $ \folderPath -> T.pack <$> readFile (folderPath </> fileName)

doRender :: Contract -> T.Text
doRender contract = T.pack (printContract contract)

getSolidity :: Contract -> T.Text
getSolidity contract = T.unlines [SolidityLibrary.headers, SolidityLibrary.library, compiledContract]
  where (compiledContract,_) = Solidity.compileContract contract

makePackage :: Contract -> IO T.Text
makePackage contract = do
  let (compiledContract,numDecisions) = Solidity.compileContract contract
  let solidityCode = T.unlines [SolidityLibrary.headers, SolidityLibrary.library, compiledContract]
  bin <- getSolcOutput solidityCode "WrapperContract.bin"
  abi <- getSolcOutput solidityCode "WrapperContract.abi"
  return $ packageJson (T.pack $ printContract contract) ("0x" `T.append` bin) abi numDecisions

packageJson :: T.Text -> T.Text -> T.Text -> Int -> T.Text
packageJson name bin abi decisions = [text|
{
  "name": "${name}",
  "bin": "${bin}",
  "abi": ${abi},
  "decisions": ${decisions'}
}
|]
  where decisions' = showt decisions

staticContracts :: IO T.Text
staticContracts =
  runSolc SolidityLibrary.fullLibrary $ \folderPath -> do
    marketplaceBin <- T.pack <$> readFile (folderPath </> "Marketplace.bin")
    marketplaceAbi <- T.pack <$> readFile (folderPath </> "Marketplace.abi")
    baseContractAbi <- T.pack <$> readFile (folderPath </> "BaseContract.abi")
    userBoolObservableBin <- T.pack <$> readFile (folderPath </> "UserBoolObservable.bin")
    userBoolObservableAbi <- T.pack <$> readFile (folderPath </> "UserBoolObservable.abi")
    userIntObservableBin <- T.pack <$> readFile (folderPath </> "UserIntObservable.bin")
    userIntObservableAbi <- T.pack <$> readFile (folderPath </> "UserIntObservable.abi")
    return
      [text|
      export const MarketplaceBin: string = "0x${marketplaceBin}";
      export const MarketplaceAbi = ${marketplaceAbi};
      export const BaseContractAbi = ${baseContractAbi};
      export const UserBoolObservableBin = "0x${userBoolObservableBin}";
      export const UserBoolObservableAbi = ${userBoolObservableAbi};
      export const UserIntObservableBin = "0x${userIntObservableBin}";
      export const UserIntObservableAbi = ${userIntObservableAbi};
      |]
