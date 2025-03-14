{-# LANGUAGE TemplateHaskell #-}

module Model.RadioGroup (RadioGroup (..), newItems) where

import Control.Monad (void, when)
import Data.Aeson.TH (deriveJSON)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Foreign.C (CULong)
import GHC.Generics (Generic)
import GI.GObject qualified as GI
import GI.Gtk qualified as Gtk
import Model.Common (runCommand)
import Model.Internal (options)
import Model.RadioButton (RadioButton (..))
import Model.RadioButton qualified as RadioButton

data RadioGroup = MkRadioGroup
  { raButtons :: NonEmpty RadioButton,
    raDefault :: Text,
    raOnGetStatus :: Text
  }
  deriving stock (Eq, Generic, Show)

$(deriveJSON options ''RadioGroup)

newItems :: Bool -> RadioGroup -> IO (NonEmpty Gtk.RadioMenuItem, IO ())
newItems verbose MkRadioGroup {..} = do
  items@(x :| xs) <- traverse (RadioButton.newItem raDefault) raButtons

  traverse_ (`Gtk.radioMenuItemJoinGroup` Just x) xs

  signalIds <- traverse connectOnToggled $ NE.zip items raButtons

  pure (items, updateAction $ NE.zip items signalIds)
  where
    connectOnToggled (item, MkRadioButton {..}) = do
      checkMenuItem <- Gtk.toCheckMenuItem item
      runCommandOnToggleActive verbose raOnClick checkMenuItem

    updateAction itemsAndSignalIds = do
      mbOutput <- runCommand verbose raOnGetStatus
      let selected = fromMaybe raDefault mbOutput
      traverse_ (updateActive selected) itemsAndSignalIds

    updateActive selected (item, signalId) = do
      label <- Gtk.menuItemGetLabel item
      GI.signalHandlerBlock item signalId
      Gtk.setCheckMenuItemActive item $ label == selected
      GI.signalHandlerUnblock item signalId

runCommandOnToggleActive :: Bool -> Text -> Gtk.CheckMenuItem -> IO CULong
runCommandOnToggleActive verbose command item =
  Gtk.on item #toggled $ do
    isActive <- Gtk.checkMenuItemGetActive item
    when isActive . void $ runCommand verbose command
