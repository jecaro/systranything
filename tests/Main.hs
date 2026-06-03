module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Tests.Model.Common qualified as Model.Common
import Tests.Model.Root qualified as Model.Root

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Model.Common.test,
        Model.Root.test
      ]
