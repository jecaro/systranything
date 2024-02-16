{-# LANGUAGE TemplateHaskell #-}

module Model.RadioGroup (RadioGroup (..), newItems) where

import Data.Aeson.TH (deriveJSON)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified GI.GObject as GI
import qualified GI.Gtk as Gtk
import Model.Checkbox (runCommandOnToggleActive)
import Model.Common (runCommand)
import Model.Internal (options)
import Model.RadioButton (RadioButton (..))
import qualified Model.RadioButton as RadioButton

data RadioGroup = MkRadioGroup
  { raButtons :: NonEmpty RadioButton,
    raDefault :: Text,
    raOnGetStatus :: Text
  }
  deriving stock (Generic, Show)

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
