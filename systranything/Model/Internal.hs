module Model.Internal (options) where

import Data.Aeson (Options(..), SumEncoding(..), camelTo2, defaultOptions)

options :: Options
options =
  defaultOptions
    { fieldLabelModifier = camelTo2 '-' . drop 2,
      constructorTagModifier = camelTo2 '-' . drop 2,
      omitNothingFields = True,
      sumEncoding = ObjectWithSingleField
    }

