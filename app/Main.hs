{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NeatInterpolation (text)
import System.IO (openFile, writeFile, IOMode(ReadMode), hGetContents, hPutStr, readFile)
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory, writeSystemTempFile, createTempDirectory, getCanonicalTemporaryDirectory)
import Control.Lens
import System.Process (callProcess, showCommandForUser)
import System.FilePath ((</>), (-<.>), takeFileName, dropFileName)
import TextShow

import Control.Monad.IO.Class (liftIO)
import DynFlags
import GHC
import GHC.Paths (libdir)
import System.Directory (getTemporaryDirectory, removePathForcibly, getDirectoryContents)
import Unsafe.Coerce (unsafeCoerce)

import CommandParser
import Declarative
import Render
import qualified Solidity
import qualified SolidityLibrary
import qualified SolidityObservable

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

getContractFromInput :: Input -> IO String
getContractFromInput (FileInput path) = do
  file <- openFile path ReadMode
  hGetContents file
getContractFromInput StdInput = getContents

compileHaskellContract :: String -> IO Contract
compileHaskellContract contract = do
  moduleFile <- writeTempFile contract
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
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

generateOutput :: Contract -> OutputType -> IO T.Text
generateOutput contract Render = return $ T.pack (printContract contract)
generateOutput contract Solidity = do
  let (compiledContract,_) = Solidity.compileContract contract
  return $ T.unlines [SolidityLibrary.headers, SolidityLibrary.library, compiledContract]
generateOutput contract Package = do
  let (compiledContract,numDecisions) = Solidity.compileContract contract
  let solidity = T.unlines [SolidityLibrary.headers, SolidityLibrary.library, compiledContract]
  bin <- getSolcOutput solidity "WrapperContract.bin"
  abi <- getSolcOutput solidity "WrapperContract.abi"
  return $ createPackage (T.pack $ printContract contract) ("0x" `T.append` bin) abi numDecisions

runSolc :: T.Text -> (FilePath -> IO a) -> IO a
runSolc solidity callback = do
    filePath <- writeSystemTempFile "solidity.sol" (T.unpack solidity)
    withSystemTempDirectory "solcout" $ \folderPath -> do
      callProcess "solc" ["--overwrite", "-o", folderPath, "--optimize", "--bin", "--abi", filePath]
      callback folderPath

getSolcOutput :: T.Text -> String -> IO T.Text
getSolcOutput solidity fileName = runSolc solidity $ \folderPath -> T.pack <$> readFile (folderPath </> fileName)

createPackage :: T.Text -> T.Text -> T.Text -> Int -> T.Text
createPackage name bin abi decisions = [text|
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

writeOutput :: Output -> T.Text -> IO ()
writeOutput o t = case o of
  FileOutput path -> writeFile path (T.unpack t)
  StdOutput -> TIO.putStr t

main :: IO ()
main = do
  opts <- execParser optionParser
  case opts of
    Compile compileOpts -> do
      haskellContract <- getContractFromInput (compileOpts^.contractInput)
      compiledContract <- compileHaskellContract haskellContract
      outputText <- generateOutput compiledContract (compileOpts^.outputType)
      writeOutput (compileOpts^.output) outputText
    StaticContracts staticOpts -> do
      outputText <- case staticOpts ^. staticOutputType of
        StaticSolidity -> return SolidityLibrary.fullLibrary
        StaticTypeScript -> staticContracts
      writeOutput (staticOpts^.staticOutput) outputText


