{-# LANGUAGE TemplateHaskell #-}

module Model.Checkbox (Checkbox (..), newItem) where

import Control.Monad (void)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import qualified Data.Text as T
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

  signalId <- runCommandOnToggle verbose chOnClick item

  pure (item, updateAction item signalId)
  where
    updateAction item signalId = do
      mbOutput <- runCommand verbose chOnGetStatus
      let active = maybe False (not . T.null) mbOutput
      GObject.signalHandlerBlock item signalId
      Gtk.checkMenuItemSetActive item active
      GObject.signalHandlerUnblock item signalId

runCommandOnToggle :: Bool -> Text -> Gtk.CheckMenuItem -> IO CULong
runCommandOnToggle verbose command item =
  Gtk.on item #toggled $ void $ runCommand verbose command
