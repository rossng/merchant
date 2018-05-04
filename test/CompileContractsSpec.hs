module CompileContractsSpec (main, spec) where

import Test.Hspec
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO (hPutStr, hClose)
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)
import Control.Monad (forM_)

import Declarative
import Render
import Observable
import Compile

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

compilable :: T.Text -> IO Bool
compilable solidityCode =
  withSystemTempFile "solidity.sol" $ \filePath handle -> do
    hPutStr handle (T.unpack solidityCode)
    hClose handle
    withSystemTempDirectory "solcout" $ \folderPath -> do
      (exitCode,_,_) <- readProcessWithExitCode "solc" ["--overwrite", "-o", folderPath, "--optimize", "--bin", "--abi", filePath] ""
      case exitCode of
        ExitSuccess -> return True
        ExitFailure _ -> return False

contracts :: [Contract]
contracts =
-- primitives
  [ zero'
  , one' USD
  , give' zero'
  , and' zero' zero'
  , or' zero' zero'
  , then' zero' zero'
  , scale' (Constant 1) zero'
  , get' zero'
  , anytime' zero'
  -- zcb
  , scaleK' 10 (get' (truncate' 1000 (one' GBP)))
  -- european
  , get' (truncate' 1000 (or' (one' USD) zero'))
  -- american
  , let
      perhaps t u = truncate' t (u `or'` zero')
      opt = anytime' (perhaps 2000 u')
      u' = one' USD
    in get' (truncate' 1000 opt) `then'` opt
  -- zcb
  , when' (At 1000) (scale' (Constant 5) (one' USD))
  -- european
  , when' (At 1000) ((one' GBP) `or'` zero')
  -- american
  , anytimeO' (OAnd (Before 2000) (After 1000)) (one' USD)
  ]

spec :: Spec
spec =
  forM_ contracts $ \c ->
    describe (printContract c) $
      it "compiles" $ do
        let contract = c
        let solidity = getSolidity contract
        compilable solidity `shouldReturn` True
