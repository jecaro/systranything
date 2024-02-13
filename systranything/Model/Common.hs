module Model.Common (runCommand) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (ExitCode (..))
import System.IO (stderr, stdout)
import System.Process.Typed (runProcess, shell)

runCommand :: Bool -> Text -> IO ()
runCommand verbose command = do
  when verbose
    . T.hPutStrLn stdout
    $ "Running command: " <> command

  exit <- runProcess . shell $ T.unpack command

  when (exit /= ExitSuccess) $
    T.hPutStrLn stderr "Command failed"
