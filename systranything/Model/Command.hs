{-# LANGUAGE TemplateHaskell #-}

module Model.Command (Command (..), periodicallyUpdateIcon) where

import Control.Monad (void)
import Data.Aeson.TH (deriveJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty.Extra ((!?))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import GHC.Generics (Generic)
import qualified GI.AyatanaAppIndicator3 as AI
import qualified GI.GLib as GLib
import Model.Common (runCommand)
import Model.Internal (options)
import Text.Read (readMaybe)

data Command = MkCommand
  { coOnTimeout :: Text,
    coPoolingInterval :: Word32,
    coIcons :: [Text]
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''Command)

periodicallyUpdateIcon :: Bool -> AI.Indicator -> Text -> Command -> IO ()
periodicallyUpdateIcon verbose indicator icon MkCommand {..} =
  void . GLib.timeoutAddSeconds GLib.PRIORITY_DEFAULT coPoolingInterval $ do
    mbOutput <- runCommand verbose coOnTimeout

    let newIcon = fromMaybe icon $ fromIndex =<< toInt =<< mbOutput

    AI.indicatorSetIconFull indicator newIcon Nothing

    pure True
  where
    fromIndex = (!?) (icon :| coIcons)
    toInt = readMaybe . T.unpack
