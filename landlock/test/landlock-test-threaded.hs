module Main (main) where

import Control.Concurrent.Async (withAsyncBound)
import Test.Tasty (defaultMain, testGroup)
import ThreadedScenario (scenario)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Threaded"
      [ scenario withAsyncBound
      ]
