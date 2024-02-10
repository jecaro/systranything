{-# LANGUAGE TemplateHaskell #-}

module Model
  ( Root (..),
    Indicator (..),
    Command (..),
    Label (..),
    Checkbox (..),
    Item (..),
    RadioButton (..),
    RadioGroup (..),
  )
where

import Data.Aeson.TH (deriveJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.Internal (options)

data Root = MkRoot
  { roIndicator :: Indicator,
    roMenu :: [Item]
  }
  deriving stock (Generic, Show)

data Indicator = MkIndicator
  { inIcon :: Text,
    inCommand :: Maybe Command
  }
  deriving stock (Generic, Show)

data Command = MkCommand
  { coCommand :: Text,
    coPoolingInterval :: Int,
    coIcons :: NonEmpty Text
  }
  deriving stock (Generic, Show)

data Label = MkLabel
  { laLabel :: Text,
    laCommand :: Text
  }
  deriving stock (Generic, Show)

data Checkbox = MkCheckbox
  { chLabel :: Text,
    chCommand :: Text,
    chChecked :: Bool
  }
  deriving stock (Generic, Show)

data Item
  = MkItemLabel Label
  | MkItemCheckbox Checkbox
  | MkItemRadioGroup RadioGroup
  | MkItemSubMenu (NonEmpty Item)
  deriving stock (Generic, Show)

data RadioGroup = MkRadioGroup
  { raItems :: NonEmpty RadioButton,
    raSelected :: Text
  }
  deriving stock (Generic, Show)

data RadioButton = MkRadioButton
  { raLabel :: Text,
    raCommand :: Text
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''Label)
$(deriveJSON options ''Checkbox)
$(deriveJSON options ''RadioButton)
$(deriveJSON options ''RadioGroup)
$(deriveJSON options ''Item)
$(deriveJSON options ''Command)
$(deriveJSON options ''Indicator)
$(deriveJSON options ''Root)
