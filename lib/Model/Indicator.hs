{-# LANGUAGE TemplateHaskell #-}

module Model.Indicator (Indicator (..), newIndicator) where

import Control.Monad (void, when)
import Data.Aeson.TH (deriveJSON)
import Data.Foldable (traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified GI.AyatanaAppIndicator3 as AI
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Model.Command (Command, periodicallyUpdateIcon)
import Model.Common (runCommand)
import Model.Internal (options)

data Indicator = MkIndicator
  { inIcon :: Text,
    inCommand :: Maybe Command,
    inOnScrollUp :: Maybe Text,
    inOnScrollDown :: Maybe Text
  }
  deriving stock (Generic, Show)

$(deriveJSON options ''Indicator)

newIndicator :: Bool -> Indicator -> Maybe Gtk.Menu -> IO AI.Indicator
newIndicator verbose indicator@MkIndicator {..} menu = do
  gtkIndicator <-
    AI.indicatorNew
      "systranything"
      inIcon
      AI.IndicatorCategorySystemServices

  -- The indicator beeing not active by default means it is hidden
  AI.indicatorSetStatus gtkIndicator AI.IndicatorStatusActive
  AI.indicatorSetMenu gtkIndicator menu

  traverse_ (periodicallyUpdateIcon False gtkIndicator inIcon) inCommand

  onScrollEvent verbose gtkIndicator indicator

  pure gtkIndicator

onScrollEvent :: Bool -> AI.Indicator -> Indicator -> IO ()
onScrollEvent verbose indicator (MkIndicator {..}) = do
  -- GTK sends one scroll up or down event followed by smooth events. Getting
  -- the actual direction from smooth requires to access the scroll event
  -- itself which doesn't seem possible with the current version of
  -- libayarana-appindicator

  refDirection <- newIORef Gdk.ScrollDirectionDown

  void $ Gtk.on indicator #scrollEvent $ \_ direction -> do
    when (direction /= Gdk.ScrollDirectionSmooth) $ do
      writeIORef refDirection direction

    lastDirection <- readIORef refDirection

    traverse_ (runCommand verbose) $
      if lastDirection == Gdk.ScrollDirectionDown
        then inOnScrollDown
        else inOnScrollUp
