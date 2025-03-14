{-# LANGUAGE TemplateHaskell #-}

module Model.RadioButton (RadioButton (..), newItem) where

import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import GI.Gtk qualified as Gtk
import Model.Internal (options)

data RadioButton = MkRadioButton
  { raLabel :: Text,
    raOnClick :: Text
  }
  deriving stock (Eq, Generic, Show)

$(deriveJSON options ''RadioButton)

newItem :: Text -> RadioButton -> IO Gtk.RadioMenuItem
newItem selected MkRadioButton {..} = do
  menuItem <- Gtk.radioMenuItemNewWithLabel ([] :: [Gtk.RadioMenuItem]) raLabel
  Gtk.checkMenuItemSetActive menuItem (selected == raLabel)
  pure menuItem
