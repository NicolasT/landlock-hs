{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE LambdaCase #-}

module System.Landlock.Flags (
      toBits
    , fromBits
    , AccessFsFlag(..)
    , accessFsFlagToBit
    , accessFsFlags
    , accessFsFlagIsReadOnly
    , CreateRulesetFlag(..)
    , createRulesetFlagToBit
    , RestrictSelfFlag
    , restrictSelfFlagToBit
    , AddRuleFlag
    , addRuleFlagToBit
    ) where

#include "linux/landlock.h"

import Data.Bits (Bits, (.|.), (.&.))

import System.Landlock.Version (Version, version1)

-- | Fold a set of flags into a bitset/number.
toBits :: (Num b, Bits b, Foldable f) => f a -> (a -> b) -> b
toBits l f = foldr (\a b -> b .|. f a) 0 l

-- | Expand a bitset/number into a set of flags.
fromBits :: (Enum a, Bounded a, Num b, Bits b) => b -> (a -> b) -> [a]
fromBits b f = filter (\a -> b .&. f a /= 0) [minBound .. maxBound]

-- | Filesystem flags.
--
-- These flags enable to restrict a sandboxed process to a set of actions on
-- files and directories. Files or directories opened before the sandboxing
-- are not subject to these restrictions.
--
-- A file can only receive these access rights:
--
-- - 'AccessFsExecute'
-- - 'AccessFsWriteFile'
-- - 'AccessFsReadFile'
--
-- A directory can receive access rights related to files or directories. The
-- following access right is applied to the directory itself, and the
-- directories beneath it:
--
-- - 'AccessFsReadDir'
--
-- However, the following access rights only apply to the content of a
-- directory, not the directory itself:
--
-- - 'AccessFsRemoveDir'
-- - 'AccessFsRemoveFile'
-- - 'AccessFsMakeChar'
-- - 'AccessFsMakeDir'
-- - 'AccessFsMakeReg'
-- - 'AccessFsMakeSock'
-- - 'AccessFsMakeFifo'
-- - 'AccessFsMakeBlock'
-- - 'AccessFsMakeSym'
--
-- __Warning:__ It is currently not possible to restrict some file-related
-- actions acessible through these syscall families: @chdir@, @truncate@,
-- @stat@, @flock@, @chmod@, @chown@, @setxattr@, @utime@, @ioctl@, @fcntl@,
-- @access@. Future Landlock evolutions will enable to restrict them.
data AccessFsFlag = AccessFsExecute     -- ^ Execute a file.
                  | AccessFsWriteFile   -- ^ Open a file with write access.
                  | AccessFsReadFile    -- ^ Open a file with read access.
                  | AccessFsReadDir     -- ^ Open a directory or list its content.
                  | AccessFsRemoveDir   -- ^ Remove an empty directory or rename one
                  | AccessFsRemoveFile  -- ^ Unlink (or rename) a file.
                  | AccessFsMakeChar    -- ^ Create (or rename or link) a character device.
                  | AccessFsMakeDir     -- ^ Create (or rename) a directory.
                  | AccessFsMakeReg     -- ^ Create (or rename or link) a regular file.
                  | AccessFsMakeSock    -- ^ Create (or rename or link) a UNIX domain socket.
                  | AccessFsMakeFifo    -- ^ Create (or rename or link) a named pipe.
                  | AccessFsMakeBlock   -- ^ Create (or rename or link) a block device.
                  | AccessFsMakeSym     -- ^ Create (or rename or link) a symbolic link.

-- Note: when adding new flags, this likely means a new ABI version is
-- available. Hence, add this to 'System.Landlock.Version' (and export it
-- from 'System.Landlock'), and make sure to update both 'accessFsFlags' and
-- 'accessFsFlagIsReadOnly'.
  deriving (Show, Eq, Enum, Bounded, Ord)

-- | Retrieve a number with the appropriate 'AccessFsFlag' bit set.
accessFsFlagToBit :: Num a => AccessFsFlag -> a
accessFsFlagToBit = \case
    AccessFsExecute -> #{const LANDLOCK_ACCESS_FS_EXECUTE}
    AccessFsWriteFile -> #{const LANDLOCK_ACCESS_FS_WRITE_FILE}
    AccessFsReadFile -> #{const LANDLOCK_ACCESS_FS_READ_FILE}
    AccessFsReadDir -> #{const LANDLOCK_ACCESS_FS_READ_DIR}
    AccessFsRemoveDir -> #{const LANDLOCK_ACCESS_FS_REMOVE_DIR}
    AccessFsRemoveFile -> #{const LANDLOCK_ACCESS_FS_REMOVE_FILE}
    AccessFsMakeChar -> #{const LANDLOCK_ACCESS_FS_MAKE_CHAR}
    AccessFsMakeDir -> #{const LANDLOCK_ACCESS_FS_MAKE_DIR}
    AccessFsMakeReg -> #{const LANDLOCK_ACCESS_FS_MAKE_REG}
    AccessFsMakeSock -> #{const LANDLOCK_ACCESS_FS_MAKE_SOCK}
    AccessFsMakeFifo -> #{const LANDLOCK_ACCESS_FS_MAKE_FIFO}
    AccessFsMakeBlock -> #{const LANDLOCK_ACCESS_FS_MAKE_BLOCK}
    AccessFsMakeSym -> #{const LANDLOCK_ACCESS_FS_MAKE_SYM}

-- | All 'AccessFsFlag' flags keyed by a Landlock ABI 'Version'.
accessFsFlags :: [(Version, [AccessFsFlag])]
accessFsFlags = [
      (version1, [AccessFsExecute .. AccessFsMakeSym])
    ]

-- | Predicate for read-only 'AccessFsFlag' flags.
accessFsFlagIsReadOnly :: AccessFsFlag -> Bool
accessFsFlagIsReadOnly = \case
    AccessFsExecute -> True
    AccessFsWriteFile -> False
    AccessFsReadFile -> True
    AccessFsReadDir -> True
    AccessFsRemoveDir -> False
    AccessFsRemoveFile -> False
    AccessFsMakeChar -> False
    AccessFsMakeDir -> False
    AccessFsMakeReg -> False
    AccessFsMakeSock -> False
    AccessFsMakeFifo -> False
    AccessFsMakeBlock -> False
    AccessFsMakeSym -> False


-- | Flags passed to @landlock_create_ruleset@.
--
-- In the current kernel API, only @LANDLOCK_CREATE_RULESET_VERSION@ is
-- defined, which should not be used when creating an actual Landlock
-- encironment (cf. 'landlock').
data CreateRulesetFlag = CreateRulesetVersion
  deriving (Show, Eq, Enum, Bounded)

createRulesetFlagToBit :: Num a => CreateRulesetFlag -> a
createRulesetFlagToBit = \case
    CreateRulesetVersion -> #{const LANDLOCK_CREATE_RULESET_VERSION}

-- | Flags passed to @landlock_restrict_self@.
--
-- In the current kernel API, no such flags are defined, hence this is a type
-- without any constructors.
data RestrictSelfFlag
  deriving (Show, Eq)

restrictSelfFlagToBit :: RestrictSelfFlag -> a
restrictSelfFlagToBit = \case {}

-- | Flags passed to @landlock_add_rule@.
--
-- In the current kernel API, no such flags are defined, hence this is a type
-- without any constructors.
data AddRuleFlag
  deriving (Show, Eq)

addRuleFlagToBit :: AddRuleFlag -> a
addRuleFlagToBit = \case {}
