module Model.Common (runCommand, runCommand') where

import Control.Monad (when)
import Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import System.Exit (ExitCode (..))
import System.Process.Typed (readProcess, shell)

runCommand :: Bool -> Text -> IO (Maybe Text)
runCommand = runCommand' T.putStrLn

-- | Takes a print function, to be able to test the produced output
runCommand' :: (Text -> IO ()) -> Bool -> Text -> IO (Maybe Text)
runCommand' output verbose command = do
  when verbose . output $ "Running command: " <> command

  (exitCode, stdoutBS, stderrBS) <- readProcess . shell $ T.unpack command

  let stdoutTxt = T.decodeUtf8Lenient (LBS.toStrict stdoutBS)

  when (exitCode /= ExitSuccess) $ do
    output "Command failed: "
    output command

  when (verbose || exitCode /= ExitSuccess) $ do
    let stderrTxt = T.decodeUtf8Lenient (LBS.toStrict stderrBS)
    output "stdout: "
    output stdoutTxt
    output "stderr: "
    output stderrTxt

  pure $
    if exitCode /= ExitSuccess
      then Nothing
      else Just $ T.stripEnd stdoutTxt
