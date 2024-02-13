{-# LANGUAGE TemplateHaskell #-}

module Model.Label (Label (..), newItem) where

import Control.Monad (void)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified GI.Gtk as Gtk
import Model.Common (runCommand)
import Model.Internal (options)

data Label = MkLabel
  { laLabel :: Text,
    laCommand :: Text
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''Label)

newItem :: Bool -> Label -> IO Gtk.MenuItem
newItem verbose MkLabel {..} = do
  menuItem <- Gtk.menuItemNewWithLabel laLabel
  void . Gtk.on menuItem #activate . void $ runCommand verbose laCommand
  pure menuItem
