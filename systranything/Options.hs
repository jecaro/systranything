module Options (Options (..), parseArgs) where

import Control.Applicative ((<**>))
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    short,
    strOption,
    switch,
  )

data Options = MkOptions
  { opVerbose :: Bool,
    opFilename :: FilePath
  }
  deriving (Show)

parseArgs :: IO Options
parseArgs = execParser (info (parser <**> helper) fullDesc)

parser :: Parser Options
parser =
  MkOptions
    <$> switch
      ( long "verbose"
          <> short 'v'
          <> help "Enable verbose mode"
      )
    <*> strOption
      ( long "filename"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "Path to the configuration file"
      )
