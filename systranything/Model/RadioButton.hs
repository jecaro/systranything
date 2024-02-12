{-# LANGUAGE TemplateHaskell #-}

module Model.RadioButton (RadioButton (..)) where

import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.Internal (options)

data RadioButton = MkRadioButton
  { raLabel :: Text,
    raCommand :: Text
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''RadioButton)
