{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
-- For more information, see the [Landlock homepage](https://landlock.io/) and its
-- [kernel documentation](https://docs.kernel.org/userspace-api/landlock.html).
--
-- __Note:__ as mentioned in the documentation of 'landlock', it is currently
-- not safe to use this API with the threaded RTS, or rather, it won't function
-- as expected, leading to potential security risks. See
-- [this issue](https://github.com/NicolasT/landlock-hs/issues/9) for more
-- background.

module System.Landlock (
    -- * Core API
    --
    -- | Use 'landlock' to sandbox a process.
    --
    -- $example
      landlock
    , RulesetAttr(..)
    -- ** Filesystem Access Flags
    --
    -- | Filesystem access flags to sandbox filesystem access.
    , AccessFsFlag(..)
    , accessFsFlags
    , accessFsFlagIsReadOnly
    -- ** Unsafe API
    , unsafeLandlock
    -- * Sandboxing Rules
    --
    -- | Sandboxing rules to apply.
    , Rule
    , pathBeneath
    -- * Utility Functions
    --
    -- | Various utility functions.
    , isSupported
    -- ** Landlock ABI Version
    --
    -- | Retrieve and handle the kernel's Landlock ABI version.
    , abiVersion
    , Version
    , version1
    -- ** Opening paths using @O_PATH@
    --
    -- | When creating a 'pathBeneath' rule, a file descriptor to a directory
    -- or file is needed. These can be safely @open@ed using the @O_PATH@ flag
    -- using the following functions.
    , withOpenPath
    , withOpenPathAt
    , OpenPathFlags(..)
    , defaultOpenPathFlags
    ) where

#define _GNU_SOURCE

#include <linux/landlock.h>
#include <sys/prctl.h>

import Control.Concurrent (rtsSupportsBoundThreads)
import Control.Exception.Base (handleJust)
import Control.Monad (void)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.IO.Exception (IOErrorType(UnsupportedOperation))
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable, sizeOf)
import System.IO.Error (ioeGetErrorType)
import System.Posix.IO (closeFd)
import System.Posix.Types (Fd)

import System.Landlock.Flags (AccessFsFlag(..), accessFsFlagToBit, accessFsFlags, accessFsFlagIsReadOnly, toBits)
import System.Landlock.OpenPath (OpenPathFlags(..), defaultOpenPathFlags, withOpenPath, withOpenPathAt)
import System.Landlock.Rules (Rule, ruleType, pathBeneath)
import System.Landlock.Syscalls (LandlockRulesetAttr(..), landlock_create_ruleset, landlock_add_rule, landlock_restrict_self, prctl)
import System.Landlock.Version (Version(..), version1)

-- | Retrieve the Landlock ABI version of the running system.
--
-- __Warning:__ calling this on a system without Landlock support, or with
-- Landlock disabled, will result in an exception.
abiVersion :: IO Version
abiVersion = Version <$> landlock_create_ruleset nullPtr 0 #{const LANDLOCK_CREATE_RULESET_VERSION}

-- | Check whether Landlock is supported and enabled on the running system.
--
-- __Note:__ this doesn't take the use of the threaded RTS into account, see
-- 'landlock'.
isSupported :: IO Bool
isSupported = handleJust unsupportedOperation (\() -> return False) $ do
    void abiVersion
    return True
  where
    unsupportedOperation exc = if isUnsupportedOperationError exc then Just () else Nothing
    isUnsupportedOperationError = isUnsupportedOperationErrorType . ioeGetErrorType
    isUnsupportedOperationErrorType = (== UnsupportedOperation)

-- | Ruleset attributes.
--
-- This represents a @struct landlock_ruleset_attr@ as passed to
-- @landlock_create_ruleset@.
data RulesetAttr = RulesetAttr { rulesetAttrHandledAccessFs :: [AccessFsFlag]
                                 -- ^ Actions (cf. 'AccessFsFlag') that ought to
                                 -- be handled by a ruleset and should be
                                 -- forbidden if no rule explicitly allow them.
                                 -- This is needed forbackward compatibility
                                 -- reasons.
                               }
  deriving (Show, Eq)

-- | Flags passed to @landlock_create_ruleset@.
--
-- In the current kernel API, only @LANDLOCK_CREATE_RULESET_VERSION@ is
-- defined, which should not be used when creating an actual Landlock
-- encironment (cf. 'landlock').
data CreateRulesetFlag = CreateRulesetVersion
  deriving (Show, Eq, Enum, Bounded)

-- | Handle to a Landlock ruleset. Use 'addRule' to register new rules.
newtype LandlockFd = LandlockFd { unLandlockFd :: Fd }
  deriving (Show, Eq)

createRuleset :: RulesetAttr -> [CreateRulesetFlag] -> IO LandlockFd
createRuleset attr flags = with attr' $ \attrp -> wrap <$> landlock_create_ruleset attrp (fromIntegral $ sizeOf attr') flags'
  where
    wrap = LandlockFd . fromIntegral
    attr' = LandlockRulesetAttr { landlockRulesetAttrHandledAccessFs = handledAccessFs }
    handledAccessFs = toBits (rulesetAttrHandledAccessFs attr) accessFsFlagToBit
    flags' = toBits flags $ \case
       CreateRulesetVersion -> #{const LANDLOCK_CREATE_RULESET_VERSION}

-- | Flags passed to @landlock_restrict_self@.
--
-- In the current kernel API, no such flags are defined, hence this is a type
-- without any constructors.
data RestrictSelfFlag
  deriving (Show, Eq)

restrictSelf :: LandlockFd -> [RestrictSelfFlag] -> IO ()
restrictSelf fd flags =
    void $ landlock_restrict_self (fromIntegral $ unLandlockFd fd) flags'
  where
    flags' = toBits flags $ \case {}

-- | Apply a Landlock sandbox to the current OS thread.
--
-- You most likely want to use 'landlock' instead. Due to @setxid@-style
-- issues, this won't work as expected when using multiple threads.
--
-- The provided action can be used to register Landlock 'Rule's on the given
-- instance (see 'addRule').
--
-- Once this returns, the Landlock sandbox will be in effect (see
-- @landlock_restrict_self@), and no privileged processes can be spawned
-- (@prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)@ has been invoked).
--
-- __Warning:__ calling this on a system without Landlock support, or with
-- Landlock disabled, will result in an exception.
unsafeLandlock :: (MonadMask m, MonadIO m)
               => RulesetAttr
                  -- ^ Ruleset attribute passed to @landlock_create_ruleset@.
               -> [CreateRulesetFlag]
                  -- ^ Flags passed to @landlock_create_ruleset@. Since no flags but
                  --   'CreateRulesetVersion' (@LANDLOCK_CREATE_RULESET_VERSION@) are
                  --   defined, and this flag must not be used when creating an actual
                  --   ruleset, this should be an empty list.
               -> [RestrictSelfFlag]
                  -- ^ Flags passed to @landlock_restrict_self@. Since no flags are
                  -- defined, this should be an empty list.
               -> ((Storable (Rule r) => Rule r -> [AddRuleFlag] -> m ()) -> m a)
                  -- ^ Action that will be called before the Landlock sandbox is
                  -- enforced. The provided function can be used to register
                  -- sandboxing rules (internally using @landlock_add_rule@),
                  -- given a 'Rule' and a set of 'AddRuleFlag's. However, since no
                  -- flags are currently defined, this should be an empty list.
               -> m a
                  -- ^ Result of the given action.
unsafeLandlock attr createRulesetFlags restrictSelfFlags act =
    bracket (liftIO $ createRuleset attr createRulesetFlags) (liftIO . closeFd . unLandlockFd) $ \fd -> do
        res <- act (addRule fd)
        liftIO $ do
            void $ prctl #{const PR_SET_NO_NEW_PRIVS} 1 0 0 0
            restrictSelf fd restrictSelfFlags
        return res

-- | Apply a Landlock sandbox to the current process.
--
-- The provided action can be used to register Landlock 'Rule's on the given
-- instance (see 'addRule').
--
-- Once this returns, the Landlock sandbox will be in effect (see
-- @landlock_restrict_self@), and no privileged processes can be spawned
-- (@prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)@ has been invoked).
--
-- __Warning:__ calling this on a system without Landlock support, or with
-- Landlock disabled, will result in an exception.
--
-- __Warning:__ due to @setxid@-style issues, this won't work as expected when
-- using multiple threads. Hence, this function will throw an exception when
-- used with the threaded RTS.
landlock :: (MonadFail m, MonadMask m, MonadIO m)
         => RulesetAttr
            -- ^ Ruleset attribute passed to @landlock_create_ruleset@.
         -> [CreateRulesetFlag]
            -- ^ Flags passed to @landlock_create_ruleset@. Since no flags but
            --   'CreateRulesetVersion' (@LANDLOCK_CREATE_RULESET_VERSION@) are
            --   defined, and this flag must not be used when creating an actual
            --   ruleset, this should be an empty list.
         -> [RestrictSelfFlag]
            -- ^ Flags passed to @landlock_restrict_self@. Since no flags are
            -- defined, this should be an empty list.
         -> ((Storable (Rule r) => Rule r -> [AddRuleFlag] -> m ()) -> m a)
            -- ^ Action that will be called before the Landlock sandbox is
            -- enforced. The provided function can be used to register
            -- sandboxing rules (internally using @landlock_add_rule@),
            -- given a 'Rule' and a set of 'AddRuleFlag's. However, since no
            -- flags are currently defined, this should be an empty list.
         -> m a
            -- ^ Result of the given action.
landlock attr createRulesetFlags restrictSelfFlags act = do
    if rtsSupportsBoundThreads  -- Sentinel for the threaded RTS
        then fail "landlock can't be safely used with the threaded RTS"
        else unsafeLandlock attr createRulesetFlags restrictSelfFlags act

-- | Flags passed to @landlock_add_rule@.
--
-- In the current kernel API, no such flags are defined, hence this is a type
-- without any constructors.
data AddRuleFlag
  deriving (Show, Eq)

-- | Register a new 'Rule' with a Landlock instance.
addRule :: (MonadIO m, Storable (Rule a))
        => LandlockFd
           -- ^ Handle to a Landlock instance.
        -> Rule a
           -- ^ Sandboxing 'Rule' to register with the Landlock instance.
        -> [AddRuleFlag]
           -- ^ Flags. Since no flags are defined, this should be an empty
           --   list.
        -> m ()
addRule fd rule flags = liftIO $ with rule $ \ruleAttrp ->
    void $ landlock_add_rule (fromIntegral $ unLandlockFd fd) (ruleType rule) ruleAttrp flags'
  where
    flags' = toBits flags $ \case {}

-- |
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
