{-# LANGUAGE TemplateHaskell #-}

module Model.Indicator (Indicator (..), newIndicator) where

import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified GI.AyatanaAppIndicator3 as AI
import qualified GI.Gtk as Gtk
import Model.Command (Command)
import Model.Internal (options)

data Indicator = MkIndicator
  { inIcon :: Text,
    inCommand :: Maybe Command
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''Indicator)

newIndicator :: Text -> Gtk.Menu -> IO AI.Indicator
newIndicator icon menu = do
  indicator <-
    AI.indicatorNew
      "systranything"
      icon
      AI.IndicatorCategorySystemServices

  -- The indicator beeing not active by default means it is hidden
  AI.indicatorSetStatus indicator AI.IndicatorStatusActive
  AI.indicatorSetMenu indicator $ Just menu
  pure indicator
