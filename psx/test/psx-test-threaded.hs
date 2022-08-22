module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified TestCases as T

main :: IO ()
main = defaultMain $ testGroup "Tests" [
      T.sigfillsetWrapped
    , T.psxDetected
    , T.psxSyscall3Works
    , T.psxSyscall6Works
    , T.psxWorking
    ]
