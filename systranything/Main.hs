module Main (main) where

import Control.Exception (Exception (..), SomeException)
import Data.Yaml (decodeFileThrow, prettyPrintParseException)
import Model (Root (..))
import Options (Options (..), parseArgs)
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)
import UnliftIO (handleAny)

main :: IO ()
main = do
  MkOptions {..} <- parseArgs
  handleAny exceptions $ do
    tree :: Root <- decodeFileThrow opFilename
    print tree

exceptions :: SomeException -> IO ()
exceptions e = hPutStr stderr message >> exitFailure
  where
    message
      | Just parseException <- fromException e =
          prettyPrintParseException parseException <> "\n"
      | otherwise = "Unknown exception: " <> show e
