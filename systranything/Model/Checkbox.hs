{-# LANGUAGE TemplateHaskell #-}

module Model.Checkbox (Checkbox (..)) where

import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.Internal (options)

data Checkbox = MkCheckbox
  { chLabel :: Text,
    chCommand :: Text,
    chChecked :: Bool
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''Checkbox)
