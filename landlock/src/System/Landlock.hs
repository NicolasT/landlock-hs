{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module:      System.Landlock
-- Description: Haskell bindings for the Linux kernel Landlock API
-- Copyright:   (c) Nicolas Trangez, 2022
-- License:     BSD-3-Clause
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: Linux
--
-- This library exposes Haskell bindings for the Linux kernel Landlock API.
--
-- The Linux kernel Landlock API provides unprivileged access control. The goal
-- of Landlock is to enable to restrict ambient rights (e.g. global filesystem
-- access) for a set of processes. Because Landlock is a stackable LSM, it makes
-- possible to create safe security sandboxes as new security layers in addition
-- to the existing system-wide access-controls. This kind of sandbox is expected
-- to help mitigate the security impact of bugs or unexpected/malicious
-- behaviors in user space applications. Landlock empowers any process,
-- including unprivileged ones, to securely restrict themselves.
--
-- For more information, see the [Landlock homepage](https://landlock.io/), its
-- [kernel documentation](https://docs.kernel.org/userspace-api/landlock.html)
-- and its [manual page](https://man.archlinux.org/man/landlock.7.en).
module System.Landlock
  ( -- * Core API

    -- | Use 'landlock' to sandbox a process.
    -- $example
    landlock,
    RulesetAttr (..),

    -- ** Filesystem Access Flags

    -- | Filesystem access flags to sandbox filesystem access.
    AccessFsFlag (..),
    accessFsFlags,
    accessFsFlagIsReadOnly,
    AccessNetFlag (..),
    accessNetFlags,

    -- * Sandboxing Rules

    -- | Sandboxing rules to apply.
    Rule,
    netPort,
    pathBeneath,

    -- * Utility Functions

    -- | Various utility functions.
    isSupported,

    -- ** Landlock ABI Version

    -- | Retrieve and handle the kernel's Landlock ABI version.
    abiVersion,
    Version,
    getVersion,
    version1,
    version2,
    version3,
    version4,

    -- ** Opening paths using @O_PATH@

    -- | When creating a 'pathBeneath' rule, a file descriptor to a directory
    -- or file is needed. These can be safely opened using the
    -- [@O_PATH@](https://man.archlinux.org/man/openat.2#O_PATH) flag using the
    -- following functions.
    withOpenPath,
    withOpenPathAt,
    OpenPathFlags (..),
    defaultOpenPathFlags,
  )
where

import Control.Exception.Base (handleJust)
import Control.Monad (void)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable, poke, sizeOf)
import GHC.IO.Exception (IOErrorType (UnsupportedOperation))
import System.IO.Error (ioeGetErrorType)
import System.Landlock.Flags
  ( AccessFsFlag (..),
    AccessNetFlag (..),
    AddRuleFlag,
    CreateRulesetFlag (..),
    RestrictSelfFlag,
    accessFsFlagIsReadOnly,
    accessFsFlagToBit,
    accessFsFlags,
    accessNetFlagToBit,
    accessNetFlags,
    addRuleFlagToBit,
    createRulesetFlagToBit,
    restrictSelfFlagToBit,
    toBits,
  )
import System.Landlock.OpenPath
  ( OpenPathFlags (..),
    defaultOpenPathFlags,
    withOpenPath,
    withOpenPathAt,
  )
import System.Landlock.Rules (Rule, netPort, pathBeneath, ruleType)
import System.Landlock.Syscalls
  ( LandlockRulesetAttr (..),
    landlock_add_rule,
    landlock_create_ruleset,
    landlock_restrict_self,
    pR_SET_NO_NEW_PRIVS,
    prctl,
    throwIfNonZero,
  )
import System.Landlock.Version (Version (..), version1, version2, version3, version4)
import System.Posix.IO (closeFd)
import System.Posix.Types (Fd)

-- Like @Foreign.Marshal.Utils.with@, but zeros out the memory first
with0 :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
with0 val f =
  alloca $ \ptr -> do
    let len = sizeOf val
    fillBytes ptr 0 len
    poke ptr val
    f ptr

-- | Retrieve the Landlock ABI version of the running system.
--
-- This invokes
-- [@landlock_create_ruleset@](https://man.archlinux.org/man/landlock_create_ruleset.2.en)
-- with the
-- [@LANDLOCK_CREATE_RULESET_VERSION@](https://man.archlinux.org/man/landlock_create_ruleset.2.en#LANDLOCK_CREATE_RULESET_VERSION)
-- option.
--
-- __Warning:__ calling this on a system without Landlock support, or with
-- Landlock disabled, will result in an exception.
abiVersion :: IO Version
abiVersion = Version . fromIntegral <$> landlock_create_ruleset nullPtr 0 flags
  where
    flags = toBits createRulesetFlagToBit [CreateRulesetVersion]

-- | Check whether Landlock is supported and enabled on the running system.
--
-- This calls 'abiVersion', catching relevant exceptions to return 'False' when
-- applicable.
isSupported :: IO Bool
isSupported = handleJust unsupportedOperation (\() -> return False) $ do
  void abiVersion
  return True
  where
    unsupportedOperation exc =
      if isUnsupportedOperationError exc then Just () else Nothing
    isUnsupportedOperationError =
      isUnsupportedOperationErrorType . ioeGetErrorType
    isUnsupportedOperationErrorType = (== UnsupportedOperation)

-- | Ruleset attributes.
--
-- This represents a @struct landlock_ruleset_attr@ as passed to
-- [@landlock_create_ruleset@](https://man.archlinux.org/man/landlock_create_ruleset.2.en).

{- HLINT ignore "Use newtype instead of data" -}
data RulesetAttr = RulesetAttr
  { -- | Actions (cf. 'AccessFsFlag') that ought to
    -- be handled by a ruleset and should be
    -- forbidden if no rule explicitly allow them.
    -- This is needed for backward compatibility
    -- reasons.
    rulesetAttrHandledAccessFs :: [AccessFsFlag],
    rulesetAttrHandledAccessNet :: [AccessNetFlag]
  }
  deriving (Show, Eq)

-- | Handle to a Landlock ruleset. Use 'addRule' to register new rules.
newtype LandlockFd = LandlockFd {unLandlockFd :: Fd}
  deriving (Show, Eq)

createRuleset :: RulesetAttr -> [CreateRulesetFlag] -> IO LandlockFd
createRuleset attr flags = with0 attr' $ \attrp ->
  wrap <$> landlock_create_ruleset attrp (fromIntegral $ sizeOf attr') flags'
  where
    wrap = LandlockFd . fromIntegral
    attr' =
      LandlockRulesetAttr
        { landlockRulesetAttrHandledAccessFs = handledAccessFs,
          landlockRulesetAttrHandledAccessNet = handledAccessNet
        }
    handledAccessFs = toBits accessFsFlagToBit (rulesetAttrHandledAccessFs attr)
    handledAccessNet = toBits accessNetFlagToBit (rulesetAttrHandledAccessNet attr)
    flags' = toBits createRulesetFlagToBit flags

restrictSelf :: LandlockFd -> [RestrictSelfFlag] -> IO ()
restrictSelf fd flags =
  landlock_restrict_self (fromIntegral $ unLandlockFd fd) flags'
  where
    flags' = toBits restrictSelfFlagToBit flags

-- | Apply a Landlock sandbox to the current process.
--
-- The provided action can be used to register Landlock 'Rule's on the given
-- instance (see 'addRule').
--
-- Once this returns, the Landlock sandbox will be in effect (see
-- [@landlock_restrict_self@](https://man.archlinux.org/man/landlock_restrict_self.2.en)),
-- and no privileged processes can be spawned
-- (@[prctl](https://man.archlinux.org/man/prctl.2)([PR_SET_NO_NEW_PRIVS](https://man.archlinux.org/man/prctl.2#PR_SET_NO_NEW_PRIVS), 1, 0, 0, 0)@
-- has been invoked).
--
-- __Warning:__ calling this on a system without Landlock support, or with
-- Landlock disabled, will result in an exception.
landlock ::
  (MonadMask m, MonadIO m) =>
  -- | Ruleset attribute passed to
  -- [@landlock_create_ruleset@](https://man.archlinux.org/man/landlock_create_ruleset.2.en).
  RulesetAttr ->
  -- | Flags passed to
  -- [@landlock_create_ruleset@](https://man.archlinux.org/man/landlock_create_ruleset.2.en).
  -- Since no flags but 'CreateRulesetVersion'
  -- ([@LANDLOCK_CREATE_RULESET_VERSION@](https://man.archlinux.org/man/landlock_create_ruleset.2.en#LANDLOCK_CREATE_RULESET_VERSION))
  -- are defined, and this flag must not be used when creating an actual
  -- ruleset, this should be an empty list.
  [CreateRulesetFlag] ->
  -- | Flags passed to
  -- [@landlock_restrict_self@](https://man.archlinux.org/man/landlock_restrict_self.2.en).
  -- Since no flags are defined, this should be an empty list.
  [RestrictSelfFlag] ->
  -- | Action that will be called before the Landlock sandbox is
  -- enforced. The provided function can be used to register
  -- sandboxing rules (internally using
  -- [@landlock_add_rule@](https://man.archlinux.org/man/landlock_add_rule.2.en)),
  -- given a 'Rule' and a set of 'AddRuleFlag's. However, since no
  -- flags are currently defined, this should be an empty list.
  (((Storable (Rule r)) => Rule r -> [AddRuleFlag] -> m ()) -> m a) ->
  -- | Result of the given action.
  m a
landlock attr createRulesetFlags restrictSelfFlags act =
  bracket
    (liftIO $ createRuleset attr createRulesetFlags)
    (liftIO . closeFd . unLandlockFd)
    $ \fd -> do
      res <- act (addRule fd)
      liftIO $ do
        throwIfNonZero "prtcl" $ prctl pR_SET_NO_NEW_PRIVS 1 0 0 0
        restrictSelf fd restrictSelfFlags
      return res

-- | Register a new 'Rule' with a Landlock instance.
addRule ::
  (MonadIO m, Storable (Rule a)) =>
  -- | Handle to a Landlock instance.
  LandlockFd ->
  -- | Sandboxing 'Rule' to register with the Landlock instance.
  Rule a ->
  -- | Flags. Since no flags are defined, this should be an empty
  --   list.
  [AddRuleFlag] ->
  m ()
addRule fd rule flags = liftIO $
  with0 rule $ \ruleAttrp ->
    landlock_add_rule
      (fromIntegral $ unLandlockFd fd)
      (ruleType rule)
      ruleAttrp
      flags'
  where
    flags' = toBits addRuleFlagToBit flags

-- $example
--
-- Example usage:
--
-- @
-- -- Retrieve the Landlock ABI version
-- abi <- abiVersion
--
-- -- Calculate access flag sets
-- -- Note: in production code, find the highest matching version or similar
-- let Just flags = lookup abi accessFsFlags
--     readOnlyFlags = filter accessFsFlagIsReadOnly flags
--
-- -- Sandbox the process
-- landlock (RulesetAttr flags) [] [] $ \\addRule -> do
--     -- Allow read-only access to the /usr hierarchy
--     withOpenPath "/usr" defaultOpenPathFlags{ directory = True } $ \\fd ->
--         addRule (pathBeneath fd readOnlyFlags) []
--
--     -- Allow read access to my public key
--     withOpenPath "\/home\/nicolas\/.ssh\/id_ed25519.pub" defaultOpenPathFlags $ \\fd ->
--         addRule (pathBeneath fd [AccessFsReadFile]) []
--
-- withFile "\/home\/nicolas\/.ssh\/id_ed25519.pub" ReadMode (\\fd -> putStrLn \"Success\")
-- -- Success
--
-- withFile "\/usr\/bin\/ghc" ReadMode (\\fd -> putStrLn \"Success\")
-- -- Success
--
-- openFile "\/home\/nicolas\/.ssh\/id_ed25519" ReadMode
-- -- *** Exception: \/home\/nicolas\/.ssh\/id_ed25519: openFile: permission denied (Permission denied)
-- @
