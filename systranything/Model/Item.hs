{-# LANGUAGE TemplateHaskell #-}

module Model.Item (Item (..)) where

import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import Model.Checkbox (Checkbox)
import Model.Internal (options)
import Model.Label (Label)
import Model.RadioGroup (RadioGroup)
import {-# SOURCE #-} Model.SubMenu (SubMenu)

data Item
  = MkItemLabel Label
  | MkItemCheckbox Checkbox
  | MkItemRadioGroup RadioGroup
  | MkItemSubMenu SubMenu
  deriving stock (Generic, Show)

$(deriveJSON options ''Item)
