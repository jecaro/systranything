module Main (main) where

import Control.Exception (Exception (..), SomeException)
import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Yaml (decodeFileThrow, prettyPrintParseException)
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import Model.Command (periodicallyUpdateIcon)
import Model.Indicator (Indicator (..), newIndicator)
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
    MkRoot {roIndicator = MkIndicator {..}, ..} <- decodeFileThrow opFilename

    void . Gtk.init $ Nothing

    menu <- Gtk.menuNew
    populate opVerbose menu roMenu

    indicator <- newIndicator inIcon menu
    traverse_ (periodicallyUpdateIcon opVerbose indicator inIcon) inCommand

    GLib.mainLoopRun =<< GLib.mainLoopNew Nothing False

exceptions :: SomeException -> IO ()
exceptions e = hPutStr stderr message >> exitFailure
  where
    message
      | Just parseException <- fromException e =
          prettyPrintParseException parseException <> "\n"
      | otherwise = "Unknown exception: " <> show e
