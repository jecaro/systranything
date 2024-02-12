{-# LANGUAGE TemplateHaskell #-}

module Model.Indicator (Indicator (..)) where

import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.Command (Command)
import Model.Internal (options)

data Indicator = MkIndicator
  { inIcon :: Text,
    inCommand :: Maybe Command
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''Indicator)
