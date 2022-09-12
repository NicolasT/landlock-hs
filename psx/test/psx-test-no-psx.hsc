{-# LANGUAGE CApiFFI #-}

module Main (main) where

#include "hs-psx.h"

import Data.Int (Int32)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.Ptr (Ptr)
import Foreign.Marshal (alloca)
import Foreign.Storable (Storable(..))

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

main :: IO ()
main = defaultMain $ testGroup "Tests" [
      testCase "sigfillset sets HS_PSX_SIGNAL" sigfillsetNotWrapped
    , testCase "Haskell psx is not detected" psxNotDetected
    ]

data SigsetT

instance Storable SigsetT where
    sizeOf _ = #{size sigset_t}
    alignment _ = #{alignment sigset_t}
    peek _ = error "peek not implemented"
    poke _ _ = error "poke not implemented"

foreign import capi unsafe "signal.h sigfillset"
    sigfillset :: Ptr SigsetT -> IO #{type int}

foreign import capi unsafe "signal.h sigismember"
    sigismember :: Ptr SigsetT -> #{type int} -> IO #{type int}

sigfillsetNotWrapped :: IO ()
sigfillsetNotWrapped = alloca $ \set -> do
    0 <- throwErrnoIfMinus1 "sigfillset" $ sigfillset set
    isMember <- throwErrnoIfMinus1 "sigismember" $ sigismember set #{const HS_PSX_SIGNAL}
    isMember @?= 1


foreign import capi unsafe "detect-psx.h detect_psx"
    detectPsx :: IO #{type int}

psxNotDetected :: IO ()
psxNotDetected =
    assertBool "psx detected" . not . toBool =<< detectPsx
  where
    toBool n = n /= 0
