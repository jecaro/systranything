{-# LANGUAGE TemplateHaskell #-}

module Model.Label (Label (..), newItem) where

import Control.Monad (void)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import GI.Gtk qualified as Gtk
import Model.Common (runCommand)
import Model.Internal (options)

data Label = MkLabel
  { laLabel :: Text,
    laOnClick :: Text
  }
  deriving stock (Eq, Generic, Show)

$(deriveJSON options ''Label)

newItem :: Bool -> Label -> IO Gtk.MenuItem
newItem verbose MkLabel {..} = do
  item <- Gtk.menuItemNewWithLabel laLabel
  void $ Gtk.on item #activate $ void $ runCommand verbose laOnClick
  pure item
