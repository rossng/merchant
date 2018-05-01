{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NeatInterpolation (text)
import System.IO (openFile, writeFile, IOMode(ReadMode), hGetContents)
import Control.Lens

import Control.Monad.IO.Class (liftIO)
import DynFlags
import GHC
import GHC.Paths (libdir)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import Unsafe.Coerce (unsafeCoerce)

import CommandParser
import Declarative
import Render
import qualified Solidity

-- many thanks to eugenk - https://stackoverflow.com/questions/47680575/haskell-ghc-8-dynamically-load-import-module

pluginModuleNameStr :: String
pluginModuleNameStr = "TheContract"

makeContractModule :: T.Text -> String
makeContractModule contract = T.unpack [text|
  module TheContract where
  import Declarative
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

generateOutput :: Contract -> OutputType -> T.Text
generateOutput contract Render = T.pack $ printContract contract
generateOutput contract Solidity = Solidity.compileContract contract

main :: IO ()
main = do
  opts <- execParser optionParser
  haskellContract <- getContractFromInput (opts^.contractInput)
  compiledContract <- compileHaskellContract haskellContract
  let outputText = generateOutput compiledContract (opts^.outputType)
  case (opts^.output) of
    FileOutput path -> writeFile path (T.unpack outputText)
    StdOutput -> TIO.putStr outputText


