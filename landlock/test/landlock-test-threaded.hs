module Main (main) where

import Control.Concurrent.Async (withAsyncBound)
import Control.Exception.Base (try)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause, ignoreTestBecause)
import Test.Tasty.HUnit ((@?=), testCase)

import System.Landlock (RulesetAttr(..), AccessFsFlag(..), landlock, isSupported)

import ThreadedScenario (scenario)

main :: IO ()
main = do
    supported <- isSupported
    let scenario' = scenario withAsyncBound
    defaultMain $ testGroup "Threaded" [
          if supported
            then expectFailBecause "landlock_restrict_self is thread-bound" scenario'
            else ignoreTestBecause "Landlock not supported" scenario'
        , testCase "landlock throws" $ do
            exc <- try $ landlock (RulesetAttr [AccessFsReadFile]) [] [] $ \_ -> return ()
            exc @?= Left (userError "landlock can't be safely used with the threaded RTS")
        ]
