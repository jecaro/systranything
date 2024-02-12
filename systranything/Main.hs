module Main (main) where

import Control.Exception (Exception (..), SomeException)
import Control.Monad (void, (<=<))
import Data.Foldable (traverse_)
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Yaml (decodeFileThrow, prettyPrintParseException)
import qualified GI.AyatanaAppIndicator3 as AI
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import Model.Checkbox (Checkbox (..))
import Model.Indicator (Indicator (..))
import Model.Item (Item (..))
import Model.Label (Label (..))
import Model.RadioButton (RadioButton (..))
import Model.RadioGroup (RadioGroup (..))
import Model.Root (Root (..))
import Model.SubMenu (SubMenu (..))
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
    populate menu roMenu
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

populate :: (Traversable t) => Gtk.Menu -> t Item -> IO ()
populate = traverse_ . initAppendAndShow
  where
    initAppendAndShow menu = traverse_ (appendAndShow menu) <=< initItem

    appendAndShow menu item = do
      Gtk.menuShellAppend menu item
      Gtk.widgetShow item

initItem :: Item -> IO [Gtk.MenuItem]
initItem (MkItemLabel (MkLabel {..})) =
  singleton <$> Gtk.menuItemNewWithLabel laLabel
initItem (MkItemCheckbox (MkCheckbox {..})) =
  singleton <$> (Gtk.toMenuItem =<< Gtk.checkMenuItemNewWithLabel chLabel)
initItem (MkItemRadioGroup (MkRadioGroup {..})) = do
  items@(x :| xs) <- traverse radioMenuNew raItems

  traverse_ (`Gtk.radioMenuItemJoinGroup` Just x) xs

  traverse Gtk.toMenuItem (NE.toList items)
  where
    radioMenuNew =
      Gtk.radioMenuItemNewWithLabel ([] :: [Gtk.RadioMenuItem]) . raLabel
initItem (MkItemSubMenu (MkSubMenu {..})) = do
  subMenu <- Gtk.menuNew
  menu <- Gtk.menuItemNewWithLabel suLabel
  Gtk.menuItemSetSubmenu menu (Just subMenu)
  populate subMenu suItems
  pure [menu]

exceptions :: SomeException -> IO ()
exceptions e = hPutStr stderr message >> exitFailure
  where
    message
      | Just parseException <- fromException e =
          prettyPrintParseException parseException <> "\n"
      | otherwise = "Unknown exception: " <> show e
