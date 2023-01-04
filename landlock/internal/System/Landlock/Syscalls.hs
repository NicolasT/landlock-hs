{-# LANGUAGE CApiFFI #-}

module System.Landlock.Syscalls
  ( LandlockRulesetAttr (..),
    landlock_create_ruleset,
    landlock_add_rule,
    landlock_restrict_self,
    prctl,
    pR_SET_NO_NEW_PRIVS,
    throwIfNonZero,
  )
where

import Control.Monad (unless)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt (..), CLong (..), CSize (..), CULong (..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import System.IO.Error (ioeSetLocation)
import System.Landlock.Hsc
  ( Landlock_rule_type,
    U32,
    U64,
    landlock_ruleset_attr_alignment,
    landlock_ruleset_attr_peek_handled_access_fs,
    landlock_ruleset_attr_poke_handled_access_fs,
    landlock_ruleset_attr_size,
    pR_SET_NO_NEW_PRIVS,
  )

{- HLINT ignore LandlockRulesetAttr "Use newtype instead of data" -}
data LandlockRulesetAttr = LandlockRulesetAttr
  { landlockRulesetAttrHandledAccessFs :: U64
  }
  deriving (Show, Eq)

instance Storable LandlockRulesetAttr where
  sizeOf _ = landlock_ruleset_attr_size
  alignment _ = landlock_ruleset_attr_alignment
  peek ptr =
    LandlockRulesetAttr
      <$> landlock_ruleset_attr_peek_handled_access_fs ptr
  poke ptr attr =
    landlock_ruleset_attr_poke_handled_access_fs
      ptr
      (landlockRulesetAttrHandledAccessFs attr)

foreign import capi unsafe "hs-landlock.h hs_landlock_create_ruleset"
  _landlock_create_ruleset ::
    Ptr LandlockRulesetAttr ->
    CSize ->
    U32 ->
    IO CLong

{- HLINT ignore landlock_create_ruleset "Use camelCase" -}
landlock_create_ruleset ::
  Ptr LandlockRulesetAttr ->
  CSize ->
  U32 ->
  IO CLong
landlock_create_ruleset attr size flags =
  throwErrnoIfMinus1 "landlock_create_ruleset" $
    _landlock_create_ruleset attr size flags

foreign import capi unsafe "hs-landlock.h hs_landlock_add_rule"
  _landlock_add_rule ::
    CInt ->
    Landlock_rule_type ->
    Ptr a ->
    U32 ->
    IO CLong

{- HLINT ignore landlock_add_rule "Use camelCase" -}
landlock_add_rule ::
  CInt ->
  Landlock_rule_type ->
  Ptr a ->
  U32 ->
  IO ()
landlock_add_rule ruleset_fd rule_type rule_attr flags =
  throwIfNonZero "landlock_add_rule" $
    throwErrnoIfMinus1 "landlock_add_rule" $
      _landlock_add_rule ruleset_fd rule_type rule_attr flags

foreign import capi unsafe "hs-landlock.h hs_landlock_restrict_self"
  _landlock_restrict_self ::
    CInt ->
    U32 ->
    IO CLong

{- HLINT ignore landlock_restrict_self "Use camelCase" -}
landlock_restrict_self ::
  CInt ->
  U32 ->
  IO ()
landlock_restrict_self ruleset_fd flags =
  throwIfNonZero "landlock_restrict_self" $
    throwErrnoIfMinus1 "landlock_restrict_self" $
      _landlock_restrict_self ruleset_fd flags

foreign import capi unsafe "hs-landlock.h hs_landlock_prctl"
  _prctl ::
    CInt ->
    CULong ->
    CULong ->
    CULong ->
    CULong ->
    IO CInt

prctl ::
  CInt ->
  CULong ->
  CULong ->
  CULong ->
  CULong ->
  IO CInt
prctl option arg2 arg3 arg4 arg5 =
  throwErrnoIfMinus1 "prctl" $ _prctl option arg2 arg3 arg4 arg5

throwIfNonZero :: (Num a, Eq a, Show a) => String -> IO a -> IO ()
throwIfNonZero location act = do
  rc <- act
  unless (rc == 0) $
    ioError $
      flip ioeSetLocation location $
        userError $
          "Unexpected return value: " ++ show rc ++ " /= 0"
