{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Concurrent.Async (withAsync)
import Control.Exception.Base (handleJust)
import Control.Monad (unless)
import Data.List (nub, sort, (\\))
import Data.Proxy (Proxy (Proxy))
import Network.Socket (PortNumber)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (IOMode (..), withFile)
import System.IO.Error (isPermissionError)
import System.Landlock
  ( AccessFsFlag (..),
    AccessNetFlag (..),
    OpenPathFlags (..),
    RulesetAttr (..),
    abiVersion,
    accessFsFlags,
    defaultOpenPathFlags,
    isSupported,
    landlock,
    version1,
    version2,
    version3,
    withOpenPath,
  )
import System.Landlock.Flags
  ( CreateRulesetFlag,
    accessFsFlagToBit,
    createRulesetFlagToBit,
  )
import System.Landlock.Rules (Rule, RuleType (..), netPort, pathBeneath)
import System.Landlock.Syscalls (LandlockRulesetAttr (..))
import System.Landlock.Version (Version (..))
import System.Posix.Types (Fd)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.QuickCheck ((=/=), (===))
import Test.QuickCheck.Classes.Base
  ( Laws (..),
    boundedEnumLaws,
    eqLaws,
    ordLaws,
    showLaws,
    storableLaws,
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, testCaseSteps, (@=?), (@?), (@?=))
import Test.Tasty.QuickCheck (Arbitrary (..), arbitraryBoundedEnum, testProperty)
import ThreadedScenario (scenario)

-- This test-suite is a bit "weird". We want to test various privilege-related
-- functions. Now, whenever we drop some privileges, we can't (and shouldn't be
-- able to) regain these later. Hence, all tests which drop privileges *must
-- run in a different process*. This sounds simple: just `fork`, run the test
-- in the subprocess, and wait for it to exit. However, this doesn't work that
-- well in a GHC world, where `forkProcess` should always immediately `exec`
-- something else: running Haskell code after `forkProcess` can lock up
-- indefinitely.
-- So, instead of using a simple `fork`, this test executable is used in two
-- ways: either as-is, in which case Tasty is used to run a bunch of tests,
-- or with the `LANDLOCK_TEST` environment variable set. If the latter is
-- set, the Tasty test-suite won't be executed, but instead a specific test
-- will run. This way, the Tasty test-suite can run this very same binary
-- in a different environment to run test scenarios.

landlockTestEnvironmentVariable :: String
landlockTestEnvironmentVariable = "LANDLOCK_TEST"

main :: IO ()
main =
  lookupEnv "LANDLOCK_TEST" >>= \case
    Nothing -> do
      defaultMain tests
    Just testName -> case lookup testName functionalTestCases of
      Nothing -> fail $ "Unknown test: " ++ testName
      Just act -> act

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ properties,
      unitTests,
      functionalTests,
      scenario withAsync
    ]

properties :: TestTree
properties =
  testGroup
    "Properties"
    [ testGroup "LandlockRulesetAttr" $
        map
          lawsToTestTree
          [ eqLaws (Proxy @LandlockRulesetAttr),
            showLaws (Proxy @LandlockRulesetAttr),
            storableLaws (Proxy @LandlockRulesetAttr)
          ],
      testGroup "RulesetAttr" $
        map
          lawsToTestTree
          [ eqLaws (Proxy @RulesetAttr),
            showLaws (Proxy @RulesetAttr)
          ],
      testGroup "CreateRulesetFlag" $
        map
          lawsToTestTree
          [ eqLaws (Proxy @CreateRulesetFlag),
            showLaws (Proxy @CreateRulesetFlag)
            -- boundedEnum laws don't work nicely with single-constructor enums
            -- , boundedEnumLaws (Proxy @CreateRulesetFlag)
          ],
      testGroup "AccessFsFlag" $
        map
          lawsToTestTree
          [ eqLaws (Proxy @AccessFsFlag),
            showLaws (Proxy @AccessFsFlag),
            boundedEnumLaws (Proxy @AccessFsFlag),
            ordLaws (Proxy @AccessFsFlag)
          ],
      testGroup "OpenPathFlags" $
        map
          lawsToTestTree
          [ eqLaws (Proxy @OpenPathFlags),
            showLaws (Proxy @OpenPathFlags)
          ],
      testGroup "Rule 'PathBeneath" $
        map
          lawsToTestTree
          [ eqLaws (Proxy @(Rule 'PathBeneath)),
            showLaws (Proxy @(Rule 'PathBeneath)),
            storableLaws (Proxy @(Rule 'PathBeneath))
          ],
      testGroup "Version" $
        map
          lawsToTestTree
          [ eqLaws (Proxy @Version),
            showLaws (Proxy @Version),
            ordLaws (Proxy @Version)
          ],
      testProperty "accessFsFlagToBit is different for different flags" $
        mapEq accessFsFlagToBit,
      testProperty "createRulesetFlagToBit is different for different flags" $
        mapEq createRulesetFlagToBit
    ]
  where
    lawsToTestTree laws = testGroup (lawsTypeclass laws) $ flip map (lawsProperties laws) $ uncurry testProperty
    mapEq fn a b =
      let cmp = if a == b then (===) else (=/=)
       in fn a `cmp` fn b

instance Arbitrary Version where
  arbitrary = Version <$> arbitrary

instance Arbitrary OpenPathFlags where
  arbitrary = OpenPathFlags <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary RulesetAttr where
  arbitrary = RulesetAttr <$> arbitrary <*> arbitrary

instance Arbitrary CreateRulesetFlag where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LandlockRulesetAttr where
  arbitrary = LandlockRulesetAttr <$> arbitrary <*> arbitrary

instance Arbitrary (Rule 'PathBeneath) where
  arbitrary =
    pathBeneath
      <$> fmap (fromIntegral :: Int -> Fd) arbitrary
      <*> fmap (nub . sort) arbitrary

instance Arbitrary (Rule 'NetPort) where
  arbitrary =
    netPort
      <$> fmap (fromIntegral :: Int -> PortNumber) arbitrary
      <*> fmap (nub . sort) arbitrary

instance Arbitrary AccessFsFlag where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AccessNetFlag where
  arbitrary = arbitraryBoundedEnum

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testCase "lookup version1 accessFsFlags" $
        lookup version1 accessFsFlags @?= Just [AccessFsExecute .. AccessFsMakeSym],
      testCase "lookup version2 accessFsFlags" $
        lookup version2 accessFsFlags @?= Just [AccessFsExecute .. AccessFsRefer],
      testCase "lookup version3 accessFsFlags" $
        lookup version3 accessFsFlags @?= Just [AccessFsExecute .. AccessFsTruncate],
      testCase "ABI v2 introduced [AccessFsRefer]" $
        (\\)
          <$> lookup version2 accessFsFlags
          <*> lookup version1 accessFsFlags
          @?= Just [AccessFsRefer],
      testCase "ABI v3 introduced [AccessFsTruncate]" $
        (\\)
          <$> lookup version3 accessFsFlags
          <*> lookup version2 accessFsFlags
          @?= Just [AccessFsTruncate],
      testCase "accessFsFlagToBit is unique for all AccessFsFlags" $
        length (nub (sort (map accessFsFlagToBit [minBound .. maxBound])))
          @?= length [minBound :: AccessFsFlag .. maxBound],
      testCase "createRulesetFlagToBit is unique for all CreateRulesetFlags" $
        length (nub (sort (map createRulesetFlagToBit [minBound .. maxBound])))
          @?= length [minBound :: CreateRulesetFlag .. maxBound]
    ]

functionalTests :: TestTree
functionalTests =
  testGroup "Functional Tests" $
    [ testCase "abiVersion >= 1" $ do
        v <- abiVersion
        v >= version1 @? "Unexpected version",
      testCase "abiVersion is idempotent" $ do
        v1 <- abiVersion
        v2 <- abiVersion
        v1 @=? v2,
      testCase "isSupported" $
        isSupported >>= assertBool "landlock API not supported"
    ]
      ++ map (\(name, _) -> testCaseSteps name (`runFunctionalTest` name)) functionalTestCases
  where
    runFunctionalTest step name = do
      step "Running test subprocess"
      (rc, stdout, stderr) <- readCreateProcessWithExitCode (mkCreateProcess name) ""
      step $ "Test subprocess exited with " ++ show rc
      unless (null stdout) $
        step $
          "Test subprocess stdout:\n" ++ stdout
      unless (null stderr) $
        step $
          "Test subprocess stderr:\n" ++ stderr
      rc @?= ExitSuccess

    mkCreateProcess name =
      (proc "/proc/self/exe" [])
        { env = Just [(landlockTestEnvironmentVariable, name)],
          close_fds = True
        }

functionalTestCases :: [(String, IO ())]
functionalTestCases =
  [ ("All v1 restrictions in sandbox", testAllV1Restrictions),
    ("Restrict read, except for /etc", testRestrictReadExceptEtc)
  ]

testAllV1Restrictions :: IO ()
testAllV1Restrictions = do
  let fn = "/etc/resolv.conf"
      try act = withFile fn ReadMode act

  -- First, try to open as-is
  try (\_ -> return ())

  -- Then, sandbox and try again
  v1Restrictions <- case lookup version1 accessFsFlags of
    Nothing -> assertFailure $ "Unknown ABI version: " ++ show version1
    Just r -> return r
  landlock (RulesetAttr v1Restrictions []) [] [] $ \_ -> return ()
  catchPermissionDenied $ try $ \_ -> fail $ "Still able to open " ++ fn

testRestrictReadExceptEtc :: IO ()
testRestrictReadExceptEtc = do
  let dir = "/etc"
      file = dir </> "passwd"
      act = withFile file ReadMode $ \_ -> return ()
  v1Restrictions <- case lookup version1 accessFsFlags of
    Nothing -> assertFailure $ "Unknown ABI version: " ++ show version1
    Just r -> return r

  act

  landlock (RulesetAttr v1Restrictions []) [] [] $ \addRule -> do
    withOpenPath dir defaultOpenPathFlags {directory = True} $ \fd -> do
      addRule (pathBeneath fd [AccessFsReadFile, AccessFsReadDir, AccessFsExecute]) []

  act

catchPermissionDenied :: IO () -> IO ()
catchPermissionDenied = handleJust (\exc -> if isPermissionError exc then Just () else Nothing) return
