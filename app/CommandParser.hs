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

data Options = Options
  { _contractInput :: Input
  , _output :: Output
  , _outputType :: OutputType
  }
makeLenses ''Options

optionParser :: ParserInfo Options
optionParser = info (options <**> helper)
  (  fullDesc
  <> progDesc "Compile the source code of CONTRACT"
  <> header "merchant - compile financial contracts for Ethereum" )

options :: Parser Options
options = Options <$> inputParser <*> outputParser <*> outputTypeParser

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
outputTypeParser = render <|> solidity

render :: Parser OutputType
render = flag' Render
  (  long "render"
  <> help "Output the rendered contract" )

solidity :: Parser OutputType
solidity = flag' Solidity
  (  long "solidity"
  <> help "Output the contract compiled to Solidity" )