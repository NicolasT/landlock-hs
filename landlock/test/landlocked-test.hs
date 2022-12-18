module Main (main) where

import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "landlocked"
    [ testCase "landlocked" $ do
        rc <- landlocked []
        case rc of
          ExitSuccess -> assertFailure "Unexpected success"
          ExitFailure _ -> return (),
      testCase "landlocked true" $ do
        rc <- landlocked ["true"]
        rc @?= ExitFailure 126,
      testCase "landlocked nosuchexecutablereally" $ do
        rc <- landlocked ["nosuchexecutablereally"]
        rc @?= ExitFailure 127,
      testCase "landlocked --ro /usr /usr/bin/true" $ do
        rc <- landlocked ["--ro", "/usr", "/usr/bin/true"]
        rc @?= ExitSuccess,
      testCase "landlocked --ro /usr true" $ do
        rc <- landlocked ["--ro", "/usr", "true"]
        rc @?= ExitSuccess,
      testCase "landlocked --ro /usr /usr/bin/false" $ do
        rc <- landlocked ["--ro", "/usr", "/usr/bin/false"]
        rc @?= ExitFailure 1,
      testCase "landlocked --ro /usr /usr/bin/touch TEMP_PATH/test" $
        withSystemTempDirectory "landlocked-test" $ \tmp -> do
          rc <- landlocked ["--ro", "/usr", "/usr/bin/touch", tmp </> "test"]
          rc @?= ExitFailure 1,
      testCase "landlocked --ro /usr --rw TEMP_PATH /usr/bin/touch TEMP_PATH/test" $
        withSystemTempDirectory "landlocked-test" $ \tmp -> do
          rc <- landlocked ["--ro", "/usr", "--rw", tmp, "/usr/bin/touch", tmp </> "test"]
          rc @?= ExitSuccess,
      testCase "landlocked --ro /usr --ro TEMP_PATH /usr/bin/touch TEMP_PATH/test" $
        withSystemTempDirectory "landlocked-test" $ \tmp -> do
          rc <- landlocked ["--ro", "/usr", "--ro", tmp, "/usr/bin/touch", tmp </> "test"]
          rc @?= ExitFailure 1,
      testCase "landlocked --version" $ do
        rc <- landlocked ["--version"]
        rc @?= ExitSuccess,
      testCase "landlocked --help" $ do
        rc <- landlocked ["--help"]
        rc @?= ExitSuccess
    ]

landlocked :: [String] -> IO ExitCode
landlocked args = do
  (rc, _, _) <- readProcessWithExitCode "landlocked" args ""
  return rc
