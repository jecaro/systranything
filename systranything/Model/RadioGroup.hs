{-# LANGUAGE TemplateHaskell #-}

module Model.RadioGroup (RadioGroup (..)) where

import Data.Aeson.TH (deriveJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.Internal (options)
import Model.RadioButton (RadioButton)

data RadioGroup = MkRadioGroup
  { raItems :: NonEmpty RadioButton,
    raSelected :: Text
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''RadioGroup)
