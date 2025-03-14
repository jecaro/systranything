{-# LANGUAGE TemplateHaskell #-}

module Model.Item (Item (..), populate) where

import Control.Monad (void)
import Data.Aeson.TH (deriveJSON)
import Data.Foldable (traverse_)
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import GHC.Generics (Generic)
import GI.Gtk qualified as Gtk
import Model.Checkbox (Checkbox (..))
import Model.Checkbox qualified as Checkbox
import Model.Internal (options)
import Model.Label (Label)
import Model.Label qualified as Label
import Model.RadioGroup (RadioGroup (..))
import Model.RadioGroup qualified as RadioGroup
import {-# SOURCE #-} Model.SubMenu (SubMenu)
import {-# SOURCE #-} Model.SubMenu qualified as SubMenu

data Item
  = MkItemLabel Label
  | MkItemCheckbox Checkbox
  | MkItemRadioGroup RadioGroup
  | MkItemSubMenu SubMenu
  | MkItemSeparator
  deriving stock (Eq, Generic, Show)

$(deriveJSON options ''Item)

populate :: Bool -> NonEmpty Item -> IO Gtk.Menu
populate verbose items = do
  menu <- Gtk.menuNew
  updateStateActions <- traverse (initAppendAndShow menu) items
  void $ Gtk.on menu #show $ sequence_ updateStateActions
  pure menu
  where
    initAppendAndShow :: Gtk.Menu -> Item -> IO (IO ())
    initAppendAndShow menu item = do
      (menuItems, updateAction) <- newItem verbose item
      traverse_ (appendAndShow menu) menuItems
      pure updateAction

    appendAndShow :: Gtk.Menu -> Gtk.MenuItem -> IO ()
    appendAndShow menu item = do
      Gtk.menuShellAppend menu item
      Gtk.widgetShow item

newItem :: Bool -> Item -> IO ([Gtk.MenuItem], IO ())
newItem verbose (MkItemLabel label) =
  (,mempty) . singleton <$> Label.newItem verbose label
newItem verbose (MkItemCheckbox checkbox) = do
  (item, updateAction) <- Checkbox.newItem verbose checkbox
  menuItems <- Gtk.toMenuItem item
  pure ([menuItems], updateAction)
newItem verbose (MkItemRadioGroup group) = do
  (items, updateAction) <- RadioGroup.newItems verbose group
  menuItems <- traverse Gtk.toMenuItem $ NE.toList items
  pure (menuItems, updateAction)
newItem verbose (MkItemSubMenu submenu) =
  (,mempty) . singleton <$> SubMenu.newItem verbose submenu
newItem _ MkItemSeparator =
  (,mempty) . singleton <$> (Gtk.toMenuItem =<< Gtk.separatorMenuItemNew)
