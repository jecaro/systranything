{-# LANGUAGE TemplateHaskell #-}

module Model.Checkbox (Checkbox (..), runOnToggle, newItem) where

import Control.Monad (void)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified GI.Gtk as Gtk
import Model.Common (runCommand)
import Model.Internal (options)

data Checkbox = MkCheckbox
  { chLabel :: Text,
    chCommandOn :: Text,
    chCommandOff :: Text,
    chChecked :: Bool
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''Checkbox)

newItem :: Bool -> Checkbox -> IO Gtk.CheckMenuItem
newItem verbose MkCheckbox {..} = do
  menuItem <- Gtk.checkMenuItemNewWithLabel chLabel
  Gtk.checkMenuItemSetActive menuItem chChecked
  runOnToggle verbose chCommandOn chCommandOff menuItem
  pure menuItem

runOnToggle :: Bool -> Text -> Text -> Gtk.CheckMenuItem -> IO ()
runOnToggle verbose commandOn commandOff menuItem =
  void . Gtk.on menuItem #toggled $ do
    isActive <- Gtk.checkMenuItemGetActive menuItem
    runCommand verbose $
      if isActive
        then commandOn
        else commandOff
