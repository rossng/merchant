module Main where

import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (writeFile, IOMode(ReadMode), openFile, hGetContents)
import Control.Lens

import CommandParser
import Compile
import qualified SolidityLibrary

writeOutput :: Output -> T.Text -> IO ()
writeOutput o t = case o of
  FileOutput path -> writeFile path (T.unpack t)
  StdOutput -> TIO.putStr t

getContractFromInput :: Input -> IO String
getContractFromInput (FileInput path) = do
  file <- openFile path ReadMode
  hGetContents file
getContractFromInput StdInput = getContents

main :: IO ()
main = do
  opts <- execParser optionParser
  case opts of
    Compile compileOpts -> do
      haskellContract <- getContractFromInput (compileOpts^.contractInput)
      dslContract <- interpretHaskell haskellContract
      outputText <- case (compileOpts^.outputType) of
        Render -> return $ doRender dslContract
        Solidity -> return $ getSolidity dslContract
        Package -> makePackage dslContract
      writeOutput (compileOpts^.output) outputText
    StaticContracts staticOpts -> do
      outputText <- case staticOpts ^. staticOutputType of
        StaticSolidity -> return SolidityLibrary.fullLibrary
        StaticTypeScript -> staticContracts
      writeOutput (staticOpts^.staticOutput) outputText


