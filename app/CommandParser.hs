{-# LANGUAGE TemplateHaskell #-}
module CommandParser where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Lens

data Input
  = FileInput FilePath
  | StdInput

data Output
  = FileOutput FilePath
  | StdOutput

data OutputType
  = Render
  | Solidity
  | Package

data StaticOutputType
  = StaticSolidity
  | StaticTypeScript

data CompileOptions = CompileOptions
  { _contractInput :: Input
  , _output :: Output
  , _outputType :: OutputType
  }
makeLenses ''CompileOptions

data StaticOptions = StaticOptions
  { _staticOutput :: Output
  , _staticOutputType :: StaticOutputType
  }
makeLenses ''StaticOptions

data Command
  = Compile CompileOptions
  | StaticContracts StaticOptions

optionParser :: ParserInfo Command
optionParser = info (commandParser <**> helper)
  (  fullDesc
  <> header "merchant - compile financial contracts for Ethereum" )

commandParser :: Parser Command
commandParser = subparser
  (  command "compile" (info compileOptions ( progDesc "Compile the source code of CONTRACT" ))
  <> command "static" (info staticOptions ( progDesc "Output the static-contracts.ts file required by the Merchant Dapp" ))
  )

staticOptions :: Parser Command
staticOptions = StaticContracts <$> (StaticOptions <$> outputParser <*> staticOutputTypeParser)

staticOutputTypeParser :: Parser StaticOutputType
staticOutputTypeParser = staticSolidity <|> staticTypescript

staticSolidity :: Parser StaticOutputType
staticSolidity = flag' StaticSolidity
  (  long "solidity"
  <> help "Output the static contracts as Solidity" )

staticTypescript :: Parser StaticOutputType
staticTypescript = flag' StaticTypeScript
  (  long "typescript"
  <> help "Output the compiled static contracts as Typescript" )

compileOptions :: Parser Command
compileOptions = Compile <$> (CompileOptions <$> inputParser <*> outputParser <*> outputTypeParser)

inputParser :: Parser Input
inputParser = fileInput <|> stdInput

fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input contract file" )

stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> help "Read contract from stdin" )

outputParser :: Parser Output
outputParser = fileOutput <|> stdOutput

fileOutput :: Parser Output
fileOutput = FileOutput <$> strOption
  (  long "output"
  <> short 'o'
  <> metavar "FILENAME"
  <> help "Output file" )

stdOutput :: Parser Output
stdOutput = flag' StdOutput
  (  long "stdout"
  <> help "Output to stdout" )

outputTypeParser :: Parser OutputType
outputTypeParser = render <|> solidity <|> package

render :: Parser OutputType
render = flag' Render
  (  long "render"
  <> help "Output the rendered contract" )

solidity :: Parser OutputType
solidity = flag' Solidity
  (  long "solidity"
  <> help "Output the contract compiled to Solidity" )

package :: Parser OutputType
package = flag' Package
  (  long "package"
  <> help "Output the contract as an Ethereum package" )