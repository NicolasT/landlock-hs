module TestCases (
      psxSyscall3Works
    , psxSyscall6Works
    , psxWorking
    , sigfillsetWrapped
    , psxDetected
    ) where

#include <signal.h>
#include <unistd.h>
#include <sys/prctl.h>
#include <sys/syscall.h>

#include <hs-psx.h>

import Control.Concurrent.Async (wait, withAsyncBound)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad (void)
import Data.Int (Int32, Int64)
import Data.Word (Word64)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=), assertBool, testCase, testCaseSteps)

foreign import ccall unsafe "hs-psx.h hs_psx_syscall3"
    psxSyscall3 :: #{type long}
                -> #{type long}
                -> #{type long}
                -> #{type long}
                -> IO #{type long}

foreign import ccall unsafe "hs-psx.h hs_psx_syscall6"
    psxSyscall6 :: #{type long}
                -> #{type long}
                -> #{type long}
                -> #{type long}
                -> #{type long}
                -> #{type long}
                -> #{type long}
                -> IO #{type long}

foreign import ccall unsafe "unistd.h getpid"
    c_getpid :: IO #{type pid_t}

getpid :: IO #{type pid_t}
getpid = throwErrnoIfMinus1 "getpid" c_getpid

foreign import ccall unsafe "unistd.h gettid"
    c_gettid :: IO #{type pid_t}

gettid :: IO #{type pid_t}
gettid = throwErrnoIfMinus1 "gettid" c_gettid

psxSyscall3Works :: TestTree
psxSyscall3Works = testCase "hs_psx_syscall3 works" $ do
    pid <- getpid
    tid <- gettid
    let sig = #{const SIGURG}
    void $ throwErrnoIfMinus1 "tgkill" $ psxSyscall3 #{const __NR_tgkill} (fromIntegral pid) (fromIntegral tid) sig

psxSyscall6Works :: TestTree
psxSyscall6Works = testCase "hs_psx_syscall6 works" $ do
    void $ throwErrnoIfMinus1 "prctl" $ psxSyscall6 #{const __NR_prctl} #{const PR_GET_NO_NEW_PRIVS} 0 0 0 0 0


foreign import ccall unsafe "sys/prctl.h prctl"
    _prctl :: #{type int}
           -> #{type unsigned long}
           -> #{type unsigned long}
           -> #{type unsigned long}
           -> #{type unsigned long}
           -> IO #{type int}

prctl :: #{type int}
      -> #{type unsigned long}
      -> #{type unsigned long}
      -> #{type unsigned long}
      -> #{type unsigned long}
      -> IO #{type int}
prctl option arg2 arg3 arg4 arg5 = throwErrnoIfMinus1 "prctl" $ _prctl option arg2 arg3 arg4 arg5

prctlPsx :: #{type int}
         -> #{type unsigned long}
         -> #{type unsigned long}
         -> #{type unsigned long}
         -> #{type unsigned long}
          -> IO #{type int}
prctlPsx option arg2 arg3 arg4 arg5 = do
    rc <- throwErrnoIfMinus1 "prctl" $
        psxSyscall6 #{const __NR_prctl} (fromIntegral option) (fromIntegral arg2) (fromIntegral arg3) (fromIntegral arg4) (fromIntegral arg5) 0
    return $ fromIntegral rc


psxWorking :: TestTree
psxWorking = testCaseSteps "psx is working" $ \step -> do
    tidMVar <- newEmptyMVar
    continueMVar <- newEmptyMVar

    step "Launching thread"
    withAsyncBound (thread step tidMVar continueMVar) $ \async -> do
        assertNoNewPrivs 0

        myTid <- gettid
        step "Waiting for thread TID"
        itsTid <- readMVar tidMVar

        step "Comparing TIDs"
        assertBool "TIDs are not different" $ myTid /= itsTid

        step "Setting PR_SET_NO_NEW_PRIVS"
        0 <- prctlPsx #{const PR_SET_NO_NEW_PRIVS} 1 0 0 0

        step "Letting thread continue"
        putMVar continueMVar ()

        assertNoNewPrivs 1

        step "Joining thread"
        wait async
  where
    getNoNewPrivs = prctl #{const PR_GET_NO_NEW_PRIVS} 0 0 0 0
    assertNoNewPrivs v =
        getNoNewPrivs >>= \r -> r @?= v

    thread step tidMVar continueMVar = do
        step "In thread" :: IO ()
        assertNoNewPrivs 0

        myTid <- gettid
        step "Sending TID"
        putMVar tidMVar myTid

        step "Waiting to continue"
        () <- readMVar continueMVar
        assertNoNewPrivs 1



data SigsetT

instance Storable SigsetT where
    sizeOf _ = #{size sigset_t}
    alignment _ = #{alignment sigset_t}
    peek _ = error "peek not implemented"
    poke _ _ = error "poke not implemented"

foreign import ccall unsafe "signal.h sigfillset"
    sigfillset :: Ptr SigsetT -> IO #{type int}

foreign import ccall unsafe "signal.h sigismember"
    sigismember :: Ptr SigsetT -> #{type int} -> IO #{type int}

sigfillsetWrapped :: TestTree
sigfillsetWrapped = testCase "sigfillset is wrapped" $ alloca $ \set -> do
    0 <- throwErrnoIfMinus1 "sigfillset" $ sigfillset set
    isMember <- throwErrnoIfMinus1 "sigismember" $ sigismember set #{const HS_PSX_SIGNAL}
    isMember @?= 0


foreign import ccall unsafe "detect-psx.h detect_psx"
    detectPsx :: IO #{type int}

psxDetected :: TestTree
psxDetected = testCase "psx detected by C code" $ do
    assertBool "psx not detected" . toBool =<< detectPsx
  where
    toBool n = n /= 0


