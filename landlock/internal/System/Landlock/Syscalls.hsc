{-# LANGUAGE CApiFFI #-}

module System.Landlock.Syscalls (
      LandlockRulesetAttr(..)
    , landlock_create_ruleset
    , landlock_add_rule
    , landlock_restrict_self
    , prctl
    , pR_SET_NO_NEW_PRIVS
    , throwIfNonZero
    ) where

#include <stddef.h>
#include <sys/prctl.h>
#include <sys/types.h>

#include "hs-landlock.h"
#include "linux/landlock.h"

import Control.Monad (unless)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import System.IO.Error (ioeSetLocation)

data LandlockRulesetAttr = LandlockRulesetAttr { landlockRulesetAttrHandledAccessFs :: #{type __u64} }
  deriving (Show, Eq)

instance Storable LandlockRulesetAttr where
  sizeOf _ = #{size struct landlock_ruleset_attr}
  alignment _ = #{alignment struct landlock_ruleset_attr}
  peek ptr = LandlockRulesetAttr <$> #{peek struct landlock_ruleset_attr, handled_access_fs} ptr
  poke ptr attr = do
      #{poke struct landlock_ruleset_attr, handled_access_fs} ptr (landlockRulesetAttrHandledAccessFs attr)

foreign import capi unsafe "hs-landlock.h hs_landlock_create_ruleset"
  _landlock_create_ruleset :: Ptr LandlockRulesetAttr
                           -> #{type size_t}
                           -> #{type __u32}
                           -> IO #{type long}

landlock_create_ruleset :: Ptr LandlockRulesetAttr
                        -> #{type size_t}
                        -> #{type __u32}
                        -> IO #{type long}
landlock_create_ruleset attr size flags =
    throwErrnoIfMinus1 "landlock_create_ruleset" $ _landlock_create_ruleset attr size flags

foreign import capi unsafe "hs-landlock.h hs_landlock_add_rule"
  _landlock_add_rule :: #{type int}
                     -> #{type enum landlock_rule_type}
                     -> Ptr a
                     -> #{type __u32}
                     -> IO #{type long}

landlock_add_rule :: #{type int}
                  -> #{type enum landlock_rule_type}
                  -> Ptr a
                  -> #{type __u32}
                  -> IO ()
landlock_add_rule ruleset_fd rule_type rule_attr flags =
    throwIfNonZero "landlock_add_rule" $
        throwErrnoIfMinus1 "landlock_add_rule" $
            _landlock_add_rule ruleset_fd rule_type rule_attr flags

foreign import capi unsafe "hs-landlock.h hs_landlock_restrict_self"
  _landlock_restrict_self :: #{type int}
                          -> #{type __u32}
                          -> IO #{type long}

landlock_restrict_self :: #{type int}
                       -> #{type __u32}
                       -> IO ()
landlock_restrict_self ruleset_fd flags =
    throwIfNonZero "landlock_restrict_self" $
        throwErrnoIfMinus1 "landlock_restrict_self" $
            _landlock_restrict_self ruleset_fd flags

foreign import capi unsafe "hs-landlock.h hs_landlock_prctl"
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
prctl option arg2 arg3 arg4 arg5 =
    throwErrnoIfMinus1 "prctl" $ _prctl option arg2 arg3 arg4 arg5

pR_SET_NO_NEW_PRIVS :: Num a => a
pR_SET_NO_NEW_PRIVS = #{const PR_SET_NO_NEW_PRIVS}

throwIfNonZero :: (Num a, Eq a, Show a) => String -> IO a -> IO ()
throwIfNonZero location act = do
    rc <- act
    unless (rc == 0) $
        ioError $ flip ioeSetLocation location
                $ userError
                $ "Unexpected return value: " ++ show rc ++ " /= 0"
