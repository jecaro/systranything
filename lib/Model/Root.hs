{-# LANGUAGE TemplateHaskell #-}

module Model.Root (Root (..)) where

import Data.Aeson.TH (deriveJSON)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Model.Indicator (Indicator)
import Model.Internal (options)
import Model.Item (Item)

data Root = MkRoot
  { roIndicator :: Indicator,
    roMenu :: Maybe (NonEmpty Item)
  }
  deriving stock (Eq, Generic, Show)

$(deriveJSON options ''Root)
