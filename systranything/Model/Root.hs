{-# LANGUAGE TemplateHaskell #-}

module Model.Root (Root (..)) where

import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import Model.Indicator (Indicator)
import Model.Internal (options)
import Model.Item (Item)

data Root = MkRoot
  { roIndicator :: Indicator,
    roMenu :: [Item]
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''Root)
