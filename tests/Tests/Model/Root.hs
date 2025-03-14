{- AUTOCOLLECT.TEST -}

module Tests.Model.Root
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text as Text (unlines)
import Data.Yaml (decodeFileThrow)
import Model.Checkbox (Checkbox (..))
import Model.Command (Command (..))
import Model.Indicator (Indicator (..))
import Model.Item (Item (..))
import Model.Label (Label (..))
import Model.RadioButton (RadioButton (..))
import Model.RadioGroup (RadioGroup (..))
import Model.Root (Root (..))
import Model.SubMenu (SubMenu (..))
import Test.Hspec.Expectations (shouldBe)
import Test.Tasty.HUnit (testCase)

test :: TestTree
test = testCase "Read tests/data/example.yaml" $ do
  root :: Root <- decodeFileThrow "tests/data/example.yaml"
  root
    `shouldBe` MkRoot
      { roIndicator =
          MkIndicator
            { inIcon = "dialog-warning",
              inCommand =
                Just
                  ( MkCommand
                      { coOnTimeout =
                          Text.unlines
                            [ "# This snippet increments a counter between zero and two. Its stores",
                              "# its state in the `indicator` file.",
                              "if [ ! -f indicator ]; then",
                              "  echo -1 > indicator",
                              "fi",
                              "",
                              "current=$(cat indicator)",
                              "next=$((($current + 1) % 3))",
                              "",
                              "echo $next > indicator",
                              "cat indicator"
                            ],
                        coPollingInterval = 3,
                        coIcons = ["dialog-error", "dialog-question"]
                      }
                  ),
              inOnScrollUp = Just "echo scroll up",
              inOnScrollDown = Just "echo scroll down"
            },
        roMenu =
          Just
            ( MkItemLabel
                ( MkLabel
                    { laLabel = "Hello",
                      laOnClick = "zenity --info --text=Hello"
                    }
                )
                :| [ MkItemCheckbox
                       ( MkCheckbox
                           { chLabel = "A checkbox",
                             chOnClick =
                               Text.unlines
                                 [ "if [ -f checkbox-file ] ; then",
                                   "  rm checkbox-file",
                                   "else",
                                   "  touch checkbox-file",
                                   "fi"
                                 ],
                             chOnGetStatus = "ls checkbox-file || true"
                           }
                       ),
                     MkItemSubMenu
                       ( MkSubMenu
                           { suLabel = "A submenu",
                             suItems =
                               MkItemRadioGroup
                                 ( MkRadioGroup
                                     { raButtons =
                                         MkRadioButton
                                           { raLabel = "No choice",
                                             raOnClick = "rm -f radio-file-1 radio-file-2\n"
                                           }
                                           :| [ MkRadioButton
                                                  { raLabel = "Choice 1",
                                                    raOnClick = "rm -f radio-file-2\ntouch radio-file-1\n"
                                                  },
                                                MkRadioButton
                                                  { raLabel = "Choice 2",
                                                    raOnClick = "rm -f radio-file-1\ntouch radio-file-2\n"
                                                  }
                                              ],
                                       raDefault = "No choice",
                                       raOnGetStatus =
                                         Text.unlines
                                           [ "if [ -f radio-file-1 ] ; then",
                                             "  echo \"Choice 1\"",
                                             "elif [ -f radio-file-2 ] ; then",
                                             "  echo \"Choice 2\"",
                                             "else",
                                             "  echo \"None\"",
                                             "fi"
                                           ]
                                     }
                                 )
                                 :| [ MkItemSeparator,
                                      MkItemLabel
                                        ( MkLabel
                                            { laLabel = "Another label",
                                              laOnClick = ""
                                            }
                                        )
                                    ]
                           }
                       ),
                     MkItemSeparator,
                     MkItemLabel
                       ( MkLabel
                           { laLabel = "Quit",
                             laOnClick = "pkill systranything"
                           }
                       )
                   ]
            )
      }
