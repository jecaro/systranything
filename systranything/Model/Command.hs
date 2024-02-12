{-# LANGUAGE TemplateHaskell #-}

module Model.Command (Command (..)) where

import Data.Aeson.TH (deriveJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.Internal (options)

data Command = MkCommand
  { coCommand :: Text,
    coPoolingInterval :: Int,
    coIcons :: NonEmpty Text
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''Command)
