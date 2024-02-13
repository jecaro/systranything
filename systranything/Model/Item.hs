{-# LANGUAGE TemplateHaskell #-}

module Model.Item (Item (..), populate) where

import Control.Monad ((<=<))
import Data.Aeson.TH (deriveJSON)
import Data.Foldable (traverse_)
import Data.List (singleton)
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)
import qualified GI.Gtk as Gtk
import Model.Checkbox (Checkbox)
import qualified Model.Checkbox as Checkbox
import Model.Internal (options)
import Model.Label (Label)
import qualified Model.Label as Label
import Model.RadioGroup (RadioGroup)
import qualified Model.RadioGroup as RadioGroup
import {-# SOURCE #-} Model.SubMenu (SubMenu)
import {-# SOURCE #-} qualified Model.SubMenu as SubMenu

data Item
  = MkItemLabel Label
  | MkItemCheckbox Checkbox
  | MkItemRadioGroup RadioGroup
  | MkItemSubMenu SubMenu
  deriving stock (Generic, Show)

$(deriveJSON options ''Item)

populate :: (Traversable t) => Bool -> Gtk.Menu -> t Item -> IO ()
populate verbose = traverse_ . initAppendAndShow
  where
    initAppendAndShow menu = traverse_ (appendAndShow menu) <=< newItem verbose

    appendAndShow menu item = do
      Gtk.menuShellAppend menu item
      Gtk.widgetShow item

newItem :: Bool -> Item -> IO [Gtk.MenuItem]
newItem verbose (MkItemLabel label) = singleton <$> Label.newItem verbose label
newItem verbose (MkItemCheckbox checkbox) =
  singleton <$> (Gtk.toMenuItem =<< Checkbox.newItem verbose checkbox)
newItem verbose (MkItemRadioGroup group) =
  traverse Gtk.toMenuItem . NE.toList =<< RadioGroup.newItems verbose group
newItem verbose (MkItemSubMenu submenu) =
  singleton <$> SubMenu.newItem verbose submenu
