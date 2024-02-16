{-# LANGUAGE TemplateHaskell #-}

module Model.Checkbox (Checkbox (..), newItem, runCommandOnToggleActive) where

import Control.Monad (void, when)
import Data.Aeson.TH (deriveJSON)
import Data.Maybe (isJust)
import Data.Text (Text)
import Foreign.C (CULong)
import GHC.Generics (Generic)
import qualified GI.GObject as GObject
import qualified GI.Gtk as Gtk
import Model.Common (runCommand)
import Model.Internal (options)

data Checkbox = MkCheckbox
  { chLabel :: Text,
    chOnClick :: Text,
    chOnGetStatus :: Text
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''Checkbox)

newItem :: Bool -> Checkbox -> IO (Gtk.CheckMenuItem, IO ())
newItem verbose MkCheckbox {..} = do
  item <- Gtk.checkMenuItemNewWithLabel chLabel

  signalId <- runCommandOnToggleActive verbose chOnClick item

  pure (item, updateAction item signalId)
  where
    updateAction item signalId = do
      mbOutput <- runCommand verbose chOnGetStatus
      GObject.signalHandlerBlock item signalId
      Gtk.checkMenuItemSetActive item $ isJust mbOutput
      GObject.signalHandlerUnblock item signalId

runCommandOnToggleActive :: Bool -> Text -> Gtk.CheckMenuItem -> IO CULong
runCommandOnToggleActive verbose command item =
  Gtk.on item #toggled $ do
    isActive <- Gtk.checkMenuItemGetActive item
    when isActive . void $ runCommand verbose command
