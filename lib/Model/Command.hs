{-# LANGUAGE TemplateHaskell #-}

module Model.Command (Command (..), periodicallyUpdateIcon) where

import Control.Monad (void)
import Data.Aeson.TH (deriveJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty.Extra ((!?))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import GHC.Generics (Generic)
import GI.AyatanaAppIndicator3 qualified as AI
import GI.GLib qualified as GLib
import Model.Common (runCommand)
import Model.Internal (options)
import Text.Read (readMaybe)

data Command = MkCommand
  { coOnTimeout :: Text,
    coPollingInterval :: Word32,
    coIcons :: [Text]
  }
  deriving stock (Eq, Generic, Show)

$(deriveJSON options ''Command)

periodicallyUpdateIcon :: Bool -> AI.Indicator -> Text -> Command -> IO ()
periodicallyUpdateIcon verbose indicator icon MkCommand {..} =
  void . GLib.timeoutAddSeconds GLib.PRIORITY_DEFAULT coPollingInterval $ do
    mbOutput <- runCommand verbose coOnTimeout

    let newIcon = fromMaybe icon $ fromIndex =<< toInt =<< mbOutput

    AI.indicatorSetIconFull indicator newIcon Nothing

    pure True
  where
    fromIndex = (!?) (icon :| coIcons)
    toInt = readMaybe . T.unpack
