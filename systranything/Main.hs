module Main (main) where

import Control.Exception (Exception (..), SomeException)
import Control.Monad (void)
import Data.Yaml (decodeFileThrow, prettyPrintParseException)
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import Model.Indicator (newIndicator)
import Model.Item (populate)
import Model.Root (Root (..))
import Options (Options (..), parseArgs)
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)
import UnliftIO (handleAny)

main :: IO ()
main = do
  MkOptions {..} <- parseArgs

  handleAny exceptions $ do
    MkRoot {..} <- decodeFileThrow opFilename

    void . Gtk.init $ Nothing

    mbMenu <- traverse (populate opVerbose) roMenu
    newIndicator opVerbose roIndicator mbMenu

    GLib.mainLoopRun =<< GLib.mainLoopNew Nothing False

exceptions :: SomeException -> IO ()
exceptions e = hPutStr stderr message >> exitFailure
  where
    message
      | Just parseException <- fromException e =
          prettyPrintParseException parseException <> "\n"
      | otherwise = "Unknown exception: " <> show e
