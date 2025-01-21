module Options (Options (..), Settings (..), parseArgs) where

import Control.Applicative (Alternative ((<|>)), (<**>))
import Options.Applicative
  ( Parser,
    execParser,
    flag',
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

data Settings = MkSettings
  { seFilename :: FilePath,
    seVerbose :: Bool
  }
  deriving (Show)

data Options = OpOptions Settings | OpVersion

parseArgs :: IO Options
parseArgs = execParser (info (parser <**> helper) fullDesc)

parser :: Parser Options
parser = OpOptions <$> settingsParser <|> OpVersion <$ versionParser

versionParser :: Parser Bool
versionParser = flag' True (long "version" <> short 'V' <> help "Show version")

settingsParser :: Parser Settings
settingsParser =
  MkSettings
    <$> strOption
      ( long "filename"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "Path to the configuration file"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Enable verbose mode"
      )
