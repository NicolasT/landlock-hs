{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Exception.Base (handleJust)
import Control.Monad (unless)
import Data.List (nub, sort)
import Data.Proxy (Proxy(Proxy))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable, peek, poke)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (IOMode(..), withFile)
import System.IO.Error (isPermissionError)
import System.Posix.Types (Fd)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)

import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit ((@?), (@=?), (@?=), testCase, testCaseSteps)
import Test.Tasty.QuickCheck as QC

import System.Landlock (AccessFsFlag(..), RulesetAttr(..), OpenPathFlags(..), abiVersion, accessFsFlags, defaultOpenPathFlags, isSupported, landlock, version1, withOpenPath)
import System.Landlock.Rules (Rule, RuleType(..), pathBeneath)
import System.Landlock.Syscalls (LandlockRulesetAttr(..))

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
main = lookupEnv "LANDLOCK_TEST" >>= \case
    Nothing -> do
        hasLandlock <- isSupported
        defaultMain (tests hasLandlock)
    Just testName -> case lookup testName functionalTestCases of
        Nothing -> fail $ "Unknown test: " ++ testName
        Just act -> act

tests :: Bool -> TestTree
tests hasLandlock = testGroup "Tests" [
      properties
    , (if hasLandlock then id else expectFailBecause "Landlock not supported") functionalTests
    ]

properties :: TestTree
properties = testGroup "Properties" [
      storable "LandlockRulesetAttr" (Proxy @LandlockRulesetAttr)
    , storable "Rule 'PathBeneath" (Proxy @(Rule 'PathBeneath))
    ]

storable :: forall proxy a. (Eq a, Show a, Arbitrary a, Storable a) => String -> proxy a -> TestTree
storable name _ = testGroup ("Storable for " ++ name) [
      QC.testProperty "peek . poke == id" $
          \a -> monadicIO $ do
              a' <- run $ alloca $ \ptr -> do
                  poke ptr (a :: a)
                  peek ptr
              assert $ a' == a
    ]

instance Arbitrary LandlockRulesetAttr where
    arbitrary = LandlockRulesetAttr <$> arbitrary

instance Arbitrary (Rule 'PathBeneath) where
    arbitrary = pathBeneath <$> fmap (fromIntegral :: Int -> Fd) arbitrary
                            <*> fmap (nub . sort) arbitrary

instance Arbitrary AccessFsFlag where
    arbitrary = arbitraryBoundedEnum


functionalTests :: TestTree
functionalTests = testGroup "Functional Tests" $ [
      testCase "abiVersion >= 1" $ do
          v <- abiVersion
          v >= version1 @? "Unexpected version"
    , testCase "abiVersion is idempotent" $ do
          v1 <- abiVersion
          v2 <- abiVersion
          v1 @=? v2
    ] ++ map (\(name, _) -> testCaseSteps name (`runFunctionalTest` name)) functionalTestCases
  where
    runFunctionalTest step name = do
        step "Running test subprocess"
        (rc, stdout, stderr) <- readCreateProcessWithExitCode (mkCreateProcess name) ""
        step $ "Test subprocess exited with " ++ show rc
        unless (null stdout) $
            step $ "Test subprocess stdout:\n" ++ stdout
        unless (null stderr) $
            step $ "Test subprocess stderr:\n" ++ stderr
        rc @?= ExitSuccess

    mkCreateProcess name = (proc "/proc/self/exe" []) { env = Just [(landlockTestEnvironmentVariable, name)]
                                                      , close_fds = True
                                                      }

functionalTestCases :: [(String, IO ())]
functionalTestCases = [
      ("All v1 restrictions in sandbox", testAllV1Restrictions)
    , ("Restrict read, except for /etc", testRestrictReadExceptEtc)
    ]

testAllV1Restrictions :: IO ()
testAllV1Restrictions = do
    let fn = "/etc/resolv.conf"
        try act = withFile fn ReadMode act

    -- First, try to open as-is
    try (\_ -> return ())

    -- Then, sandbox and try again
    let Just v1Restrictions = lookup version1 accessFsFlags
    landlock (RulesetAttr v1Restrictions) [] [] $ \_ -> return ()
    catchPermissionDenied $ try $ \_ -> fail $ "Still able to open " ++ fn

testRestrictReadExceptEtc :: IO ()
testRestrictReadExceptEtc = do
    let dir = "/etc"
        file = dir </> "passwd"
        act = withFile file ReadMode $ \_ -> return ()
        Just v1Restrictions = lookup version1 accessFsFlags

    act

    landlock (RulesetAttr v1Restrictions) [] [] $ \addRule -> do
        withOpenPath dir defaultOpenPathFlags { directory = True } $ \fd -> do
            addRule (pathBeneath fd [AccessFsReadFile, AccessFsReadDir, AccessFsExecute]) []

    act

catchPermissionDenied :: IO () -> IO ()
catchPermissionDenied = handleJust (\exc -> if isPermissionError exc then Just () else Nothing) return
