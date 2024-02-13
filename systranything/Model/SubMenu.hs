{-# LANGUAGE TemplateHaskell #-}

module Model.SubMenu (SubMenu (..), newItem) where

import Data.Aeson.TH (deriveJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified GI.Gtk as Gtk
import Model.Internal (options)
import Model.Item (Item, populate)

data SubMenu = MkSubMenu
  { suLabel :: Text,
    suItems :: NonEmpty Item
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''SubMenu)

newItem :: Bool -> SubMenu -> IO Gtk.MenuItem
newItem verbose (MkSubMenu {..}) = do
  subMenu <- Gtk.menuNew
  menu <- Gtk.menuItemNewWithLabel suLabel
  Gtk.menuItemSetSubmenu menu (Just subMenu)
  populate verbose subMenu suItems
  pure menu
