{- AUTOCOLLECT.TEST -}

module Tests.Model.Common
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Text qualified as Text
import Model.Common (runCommand')
import Test.Hspec.Expectations (shouldBe, shouldSatisfy)
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase)

test :: TestTree
test =
  testGroup
    "runCommand"
    [ testGroup
        "succeeding"
        [ testCase "outputs stdout" $ do
            result <- runCommandNoOutput False succeedingCommand
            result `shouldBe` Just "hello",
          testCase "in verbose mode prints stdout and stderr" $ do
            (_, output) <- runCommandWithOutput True succeedingCommand
            output `shouldSatisfy` containsAll ["stdout", "stderr"]
        ],
      testGroup
        "failing"
        [ testCase "returns nothing" $ do
            result <- runCommandNoOutput False failingCommand
            result `shouldBe` Nothing,
          testCase "prints the command to stdout" $ do
            (_, output) <- runCommandWithOutput False failingCommand
            output `shouldSatisfy` Text.isInfixOf "exit 1",
          testCase "prints stdout and stderr" $ do
            (_, output) <- runCommandWithOutput False failingCommand
            output `shouldSatisfy` containsAll ["stdout", "stderr"]
        ]
    ]
  where
    succeedingCommand = "echo 'hello'"
    failingCommand = "exit 1"
    runCommandNoOutput = runCommand' $ const $ pure ()
    runCommandWithOutput verbose cmd = do
      ref <- newIORef Text.empty
      result <- runCommand' (appendTo ref) verbose cmd
      (result,) <$> readIORef ref
    appendTo ref = modifyIORef ref . flip (<>)
    containsAll extracts text = all (`Text.isInfixOf` text) extracts
