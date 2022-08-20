module Main (main) where

import Control.Concurrent.Async (withAsyncBound)

import Test.Tasty (defaultMain)
import Test.Tasty.ExpectedFailure (expectFailBecause, ignoreTestBecause)

import System.Landlock (isSupported)

import ThreadedScenario (scenario)

main :: IO ()
main = do
    supported <- isSupported
    let test = scenario withAsyncBound
    defaultMain $
        if supported
            then expectFailBecause "landlock_restrict_self is thread-bound" test
            else ignoreTestBecause "Landlock not supported" test
