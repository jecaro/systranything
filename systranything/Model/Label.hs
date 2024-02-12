{-# LANGUAGE TemplateHaskell #-}

module Model.Label (Label (..)) where

import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.Internal (options)

data Label = MkLabel
  { laLabel :: Text,
    laCommand :: Text
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''Label)
