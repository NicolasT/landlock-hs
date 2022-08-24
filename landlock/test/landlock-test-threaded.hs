module Main (main) where

import Control.Concurrent.Async (withAsyncBound)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)

import System.Landlock (isSupported)

import ThreadedScenario (scenario)

main :: IO ()
main = do
    supported <- isSupported
    let scenario' = scenario withAsyncBound
    defaultMain $ testGroup "Threaded" [
          if supported
            then scenario'
            else expectFailBecause "Landlock not supported" scenario'
        ]
