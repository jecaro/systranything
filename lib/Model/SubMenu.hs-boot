module Model.SubMenu where

import Data.Aeson (FromJSON, ToJSON)
import qualified GI.Gtk as Gtk

data SubMenu

instance Show SubMenu

instance FromJSON SubMenu

instance ToJSON SubMenu

newItem :: Bool -> SubMenu -> IO Gtk.MenuItem
