module Model.Common (runCommand) where

import Control.Monad (when)
import Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.Exit (ExitCode (..))
import System.Process.Typed (readProcess, shell)

runCommand :: Bool -> Text -> IO (Maybe Text)
runCommand verbose command = do
  when verbose . T.putStrLn $ "Running command: " <> command

  (exitCode, stdoutBS, stderrBS) <- readProcess . shell $ T.unpack command

  let stdoutTxt = T.decodeUtf8Lenient (LBS.toStrict stdoutBS)

  when (exitCode /= ExitSuccess)
    . T.putStrLn
    $ "Command failed: " <> command

  when (verbose || exitCode /= ExitSuccess) $ do
    let stderrTxt = T.decodeUtf8Lenient (LBS.toStrict stderrBS)
    T.putStrLn $ "stdout: " <> stdoutTxt
    T.putStrLn $ "stderr: " <> stderrTxt

  pure $
    if exitCode /= ExitSuccess
      then Nothing
      else Just stdoutTxt
