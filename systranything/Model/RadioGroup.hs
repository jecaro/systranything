{-# LANGUAGE TemplateHaskell #-}

module Model.RadioGroup (RadioGroup (..), newItems) where

import Data.Aeson.TH (deriveJSON)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified GI.Gtk as Gtk
import Model.Checkbox (runOnToggle)
import Model.Internal (options)
import Model.RadioButton (RadioButton (..))
import qualified Model.RadioButton as RadioButton

data RadioGroup = MkRadioGroup
  { raItems :: NonEmpty RadioButton,
    raSelected :: Text
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''RadioGroup)

newItems :: Bool -> RadioGroup -> IO (NonEmpty Gtk.RadioMenuItem)
newItems verbose MkRadioGroup {..} = do
  items@(x :| xs) <- traverse (RadioButton.newItem raSelected) raItems

  traverse_ (`Gtk.radioMenuItemJoinGroup` Just x) xs

  traverse_ connect (NE.zip items raItems)

  pure items
  where
    connect (item, MkRadioButton {..}) =
      runOnToggle verbose raCommandOn raCommandOff =<< Gtk.toCheckMenuItem item
