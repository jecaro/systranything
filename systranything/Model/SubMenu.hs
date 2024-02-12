{-# LANGUAGE TemplateHaskell #-}

module Model.SubMenu (SubMenu (..)) where

import Data.Aeson.TH (deriveJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.Internal (options)
import Model.Item (Item)

data SubMenu = MkSubMenu
  { suLabel :: Text,
    suItems :: NonEmpty Item
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''SubMenu)
