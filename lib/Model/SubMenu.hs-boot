module Model.SubMenu where

import Data.Aeson (FromJSON, ToJSON)
import GI.Gtk qualified as Gtk

data SubMenu

instance Eq SubMenu

instance Show SubMenu

instance FromJSON SubMenu

instance ToJSON SubMenu

newItem :: Bool -> SubMenu -> IO Gtk.MenuItem
