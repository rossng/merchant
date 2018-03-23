{-# LANGUAGE OverloadedStrings #-}
module Ethereum where

import qualified Data.Text as T
import Data.Aeson
import Data.Time.Clock
import qualified Data.Attoparsec.ByteString as A
import System.IO
import System.Process
import qualified Data.ByteString.Char8 as BS

data TruffleContract = TruffleContract {
  contractName :: T.Text,
  abi :: Value,
  ast :: Value,
  bytecode :: T.Text,
  deployedBytecode :: T.Text,
  source :: T.Text,
  sourcePath :: T.Text,
  sourceMap :: T.Text,
  deployedSourceMap :: T.Text,
  schemaVersion :: T.Text,
  updatedAt :: UTCTime,
  networks :: Value
}

getAbi :: T.Text -> IO (A.Result Value)
getAbi source = do
  (Just hin, Just hout, _, _) <- createProcess (proc "solc" ["--abi"])
    { std_in = CreatePipe
    , std_out = CreatePipe }
  hPutStr hin (T.unpack source)
  hClose hin
  hGetLine hout
  hGetLine hout
  hGetLine hout
  abi <- hGetContents hout
  return (A.parse json (BS.pack abi))