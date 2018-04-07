{-# LANGUAGE OverloadedStrings, QuasiQuotes, ScopedTypeVariables, TemplateHaskell #-}
module Ethereum where

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Data.Time.Clock
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.Attoparsec.Combinator
import System.IO
import System.Process
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import NeatInterpolation (text)
import Data.Char
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Applicative ((<|>))
import Control.Lens
import Text.ParserCombinators.Perm
import Data.Word

data CompiledContract = CompiledContract
  { _contractName :: BS.ByteString
  , _abi :: Aeson.Value
  , _bytecode :: BS.ByteString
  , _deployedBytecode :: BS.ByteString
--  _sourcePath :: T.Text,
--  _sourceMap :: T.Text,
--  _deployedSourceMap :: T.Text,
--  _schemaVersion :: T.Text,
--  _updatedAt :: UTCTime,
--  _networks :: Aeson.Value
}
makeLenses ''TruffleContract

multiplySource :: T.Text
multiplySource = [text|
pragma solidity ^0.4.20;

contract Multiply7 {
   event Print(uint);
   function multiply(uint input) public returns (uint) {
      emit Print(input * 7);
      return input * 7;
   }
}
|]

skipLines :: Handle -> Int -> IO ()
skipLines h n
  | n <= 0 = return ()
  | otherwise = do
    hGetLine h
    skipLines h (n-1)

--getAbi :: T.Text -> IO (A.Result Value)
--getAbi source = do
--  (Just hin, Just hout, _, _) <- createProcess (proc "solc" ["--abi"])
--    { std_in = CreatePipe
--    , std_out = CreatePipe }
--  hPutStr hin (T.unpack source)
--  hClose hin
--  skipLines hout 3
--  abi <- hGetContents hout
--  return (A.parse json (BS.pack abi))
--
--getAst :: T.Text -> IO (A.Result Value)
--getAst source = do
--  (Just hin, Just hout, _, _) <- createProcess (proc "solc" ["--ast-json"])
--    { std_in = CreatePipe
--    , std_out = CreatePipe }
--  hPutStr hin (T.unpack source)
--  hClose hin
--  skipLines hout 4
--  ast <- hGetContents hout
--  return (A.parse json (BS.pack ast))

--getBytecode :: T.Text -> IO T.Text
--getBytecode source = do
--  (Just hin, Just hout, _, _) <- createProcess (proc "solc" ["--bin"])
--    { std_in = CreatePipe
--    , std_out = CreatePipe }
--  hPutStr hin (T.unpack source)
--  hClose hin
--  skipLines hout 3
--  bytecode <- hGetContents hout
--  let bytecode' = T.pack $ "0x" ++ takeWhile isAlphaNum bytecode
--  return bytecode'

--getRuntimeBytecode :: T.Text -> IO T.Text
--getRuntimeBytecode source = do
--  (Just hin, Just hout, _, _) <- createProcess (proc "solc" ["--bin-runtime"])
--    { std_in = CreatePipe
--    , std_out = CreatePipe }
--  hPutStr hin (T.unpack source)
--  hClose hin
--  skipLines hout 3
--  bytecode <- hGetContents hout
--  let bytecode' = T.pack $ "0x" ++ takeWhile isAlphaNum bytecode
--  return bytecode'

---

--compilerMessages :: AC.Parser
compilerMessagesParser = A.manyTill AC.anyChar (lookAhead $ A.string "=======")

contractTitleParser :: A.Parser BS.ByteString
contractTitleParser = do
  A.string "======= "
  A.manyTill AC.anyChar (A.string ":")
  name <- A.manyTill AC.anyChar (A.string " =======")
  AC.endOfLine
  return $ BS.pack name

isHexDigit' :: Word8 -> Bool
isHexDigit' c = B.elem c "0123456789abcdefABCDEF"

abiParser :: A.Parser Aeson.Value
abiParser = do
  A.string "Contract JSON ABI"
  AC.endOfLine
  abi <- Aeson.json
  AC.endOfLine
  return abi

runtimeBinaryParser :: A.Parser BS.ByteString
runtimeBinaryParser = do
  A.string "Binary of the runtime part:"
  AC.endOfLine
  binary <- A.takeWhile1 isHexDigit'
  AC.endOfLine
  return $ "0x" `BS.append` binary

binaryParser :: A.Parser BS.ByteString
binaryParser = do
  A.string "Binary:"
  AC.endOfLine
  binary <- A.takeWhile1 isHexDigit'
  AC.endOfLine
  return $ "0x" `BS.append` binary

contractParser :: ByteString -> A.Parser CompiledContract
contractParser = do
  name <- contractTitleParser
  permute ((TruffleContract name source)
            <$$> abiParser
            <||> runtimeBinaryParser
            <||> binaryParser)
  --let contents = foldl1 (>>) (many' (binaryParser <|> abiParser))
  --contract <- evalStateT contents (emptyPartialTruffleContract name)
  --return (reifyTruffleContract contract)
  return Nothing

sectionTitleExample :: T.Text
sectionTitleExample = [text|
======= <stdin>:MultiplyFactory =======
|]