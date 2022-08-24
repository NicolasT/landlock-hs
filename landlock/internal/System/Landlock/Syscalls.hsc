module System.Landlock.Syscalls (
      LandlockRulesetAttr(..)
    , landlock_create_ruleset
    , landlock_add_rule
    , landlock_restrict_self
    , prctl
    ) where

#include <stddef.h>
#include <linux/landlock.h>
#include <sys/prctl.h>
#include <sys/types.h>

#include "hs-landlock.h"

import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

data LandlockRulesetAttr = LandlockRulesetAttr { landlockRulesetAttrHandledAccessFs :: #{type __u64} }
  deriving (Show, Eq)

instance Storable LandlockRulesetAttr where
  sizeOf _ = #{size struct landlock_ruleset_attr}
  alignment _ = #{alignment struct landlock_ruleset_attr}
  peek ptr = LandlockRulesetAttr <$> #{peek struct landlock_ruleset_attr, handled_access_fs} ptr
  poke ptr attr = do
      #{poke struct landlock_ruleset_attr, handled_access_fs} ptr (landlockRulesetAttrHandledAccessFs attr)

foreign import ccall unsafe "hs-landlock.h landlock_create_ruleset"
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

foreign import ccall unsafe "hs-landlock.h landlock_add_rule"
  _landlock_add_rule :: #{type int}
                     -> #{type enum landlock_rule_type}
                     -> Ptr a
                     -> #{type __u32}
                     -> IO #{type long}

landlock_add_rule :: #{type int}
                  -> #{type enum landlock_rule_type}
                  -> Ptr a
                  -> #{type __u32}
                  -> IO #{type long}
landlock_add_rule ruleset_fd rule_type rule_attr flags =
    throwErrnoIfMinus1 "landlock_add_rule" $ _landlock_add_rule ruleset_fd rule_type rule_attr flags

foreign import ccall unsafe "hs-landlock.h landlock_restrict_self"
  _landlock_restrict_self :: #{type int}
                          -> #{type __u32}
                          -> IO #{type long}

landlock_restrict_self :: #{type int}
                       -> #{type __u32}
                       -> IO #{type long}
landlock_restrict_self ruleset_fd flags =
    throwErrnoIfMinus1 "landlock_restrict_self" $ _landlock_restrict_self ruleset_fd flags

foreign import ccall unsafe "hs-landlock.h hs_landlock_prctl"
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
