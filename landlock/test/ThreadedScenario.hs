{-# LANGUAGE CApiFFI #-}

module ThreadedScenario (scenario) where

import Control.Concurrent.Async (Async, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Base (handleJust)
import System.IO (IOMode (ReadMode), withFile)
import System.IO.Error (isPermissionError)
import System.Landlock (AccessFsFlag (..), RulesetAttr (..), landlock)
import System.Posix.Types (CPid (..))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCaseSteps)

foreign import capi unsafe "unistd.h gettid"
  gettid :: IO CPid

scenario :: (IO () -> (Async () -> IO ()) -> IO ()) -> TestTree
scenario fn = testCaseSteps "Multithreaded Scenario" (scenario' fn)

scenario' :: (IO () -> (Async () -> IO ()) -> IO ()) -> (String -> IO ()) -> IO ()
scenario' withAsync step = do
  step "Starting scenario"

  mainTid <- gettid
  step $ "Main TID = " ++ show mainTid

  tidMVar <- newEmptyMVar
  continueMVar <- newEmptyMVar

  step "Launching thread"
  withAsync (thread tidMVar continueMVar) $ \child -> do
    _ <- takeMVar tidMVar

    step "Setting up Landlock sandbox"
    let flags = [AccessFsReadFile]
    landlock (RulesetAttr flags []) [] [] $ \_ -> return ()

    step "Assert file not readable from main thread"
    assertFileNotReadable "main"
    step "Success"

    step "Letting thread continue"
    putMVar continueMVar ()

    step "Waiting for thread to exit"
    wait child
  where
    thread tidMVar continueMVar = do
      step "Running in thread"

      tid <- gettid
      step $ "Thread TID = " ++ show tid
      putMVar tidMVar tid

      step "Waiting for the signal..."
      () <- takeMVar continueMVar
      step "Received signal, continuing"

      step "Assert file not readable from thread"
      assertFileNotReadable "thread"
      step "Success"

    file = "/etc/resolv.conf"
    assertFileNotReadable env = handleJust permissionError return $
      withFile file ReadMode $ \_ ->
        assertFailure $ "Still able to open file " ++ file ++ " in " ++ env
    permissionError exc = if isPermissionError exc then Just () else Nothing
