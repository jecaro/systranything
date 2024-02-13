module Main (main) where

import Control.Exception (Exception (..), SomeException)
import Control.Monad (void)
import Data.Text (Text)
import Data.Yaml (decodeFileThrow, prettyPrintParseException)
import qualified GI.AyatanaAppIndicator3 as AI
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import Model.Indicator (Indicator (..))
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
    indicator <- initIndicator inIcon menu

    mainLoop <- GLib.mainLoopNew Nothing False

    -- Make sure the indicator is not garbage collected
    Gtk.withManagedPtr indicator . const $
      GLib.mainLoopRun mainLoop

initIndicator :: Text -> Gtk.Menu -> IO AI.Indicator
initIndicator icon menu = do
  indicator <-
    AI.indicatorNew
      "systranything"
      icon
      AI.IndicatorCategorySystemServices

  -- The indicator beeing not active by default means it is hidden
  AI.indicatorSetStatus indicator AI.IndicatorStatusActive
  AI.indicatorSetMenu indicator $ Just menu
  pure indicator

exceptions :: SomeException -> IO ()
exceptions e = hPutStr stderr message >> exitFailure
  where
    message
      | Just parseException <- fromException e =
          prettyPrintParseException parseException <> "\n"
      | otherwise = "Unknown exception: " <> show e
