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

import Data.Bits (Bits, (.|.), (.&.))

import System.Landlock.Hsc (
      U32
    , U64
    , lANDLOCK_CREATE_RULESET_VERSION
    , lANDLOCK_ACCESS_FS_EXECUTE
    , lANDLOCK_ACCESS_FS_WRITE_FILE
    , lANDLOCK_ACCESS_FS_READ_FILE
    , lANDLOCK_ACCESS_FS_READ_DIR
    , lANDLOCK_ACCESS_FS_REMOVE_DIR
    , lANDLOCK_ACCESS_FS_REMOVE_FILE
    , lANDLOCK_ACCESS_FS_MAKE_CHAR
    , lANDLOCK_ACCESS_FS_MAKE_DIR
    , lANDLOCK_ACCESS_FS_MAKE_REG
    , lANDLOCK_ACCESS_FS_MAKE_SOCK
    , lANDLOCK_ACCESS_FS_MAKE_FIFO
    , lANDLOCK_ACCESS_FS_MAKE_BLOCK
    , lANDLOCK_ACCESS_FS_MAKE_SYM
    , lANDLOCK_ACCESS_FS_REFER
    )
import System.Landlock.Version (Version, version1, version2)

-- | Fold a set of flags into a bitset/number.
toBits :: (Num b, Bits b, Foldable f) => (a -> b) -> f a -> b
toBits f = foldr (\a b -> b .|. f a) 0

-- | Expand a bitset/number into a set of flags.
fromBits :: (Enum a, Bounded a, Num b, Bits b) => (a -> b) -> b -> [a]
fromBits f b = filter (\a -> b .&. f a /= 0) [minBound .. maxBound]

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
-- - 'AccessFsRefer'
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
                  | AccessFsRefer       -- ^ Link or rename a file from or to a different
                                        --   directory (i.e. reparent a file hierarchy).  This access right is
                                        --   available since the second version of the Landlock ABI.  This is also the
                                        --   only access right which is always considered handled by any ruleset in
                                        --   such a way that reparenting a file hierarchy is always denied by default.
                                        --   To avoid privilege escalation, it is not enough to add a rule with this
                                        --   access right.  When linking or renaming a file, the destination directory
                                        --   hierarchy must also always have the same or a superset of restrictions of
                                        --   the source hierarchy.  If it is not the case, or if the domain doesn't
                                        --   handle this access right, such actions are denied by default with @errno@
                                        --   set to @EXDEV@.  Linking also requires a @LANDLOCK_ACCESS_FS_MAKE_*@ access
                                        --   right on the destination directory, and renaming also requires a
                                        --   @LANDLOCK_ACCESS_FS_REMOVE_*@ access right on the source's (file or
                                        --   directory) parent.  Otherwise, such actions are denied with @errno@ set to
                                        --   @EACCES@.  The @EACCES@ @errno@ prevails over @EXDEV@ to let user space
                                        --   efficiently deal with an unrecoverable error.

-- Note: when adding new flags, this likely means a new ABI version is
-- available. Hence, add this to 'System.Landlock.Version' (and export it
-- from 'System.Landlock'), and make sure to update both 'accessFsFlags' and
-- 'accessFsFlagIsReadOnly'.
  deriving (Show, Eq, Enum, Bounded, Ord)

-- | Retrieve a number with the appropriate 'AccessFsFlag' bit set.
accessFsFlagToBit :: AccessFsFlag -> U64
accessFsFlagToBit = \case
    AccessFsExecute -> lANDLOCK_ACCESS_FS_EXECUTE
    AccessFsWriteFile -> lANDLOCK_ACCESS_FS_WRITE_FILE
    AccessFsReadFile -> lANDLOCK_ACCESS_FS_READ_FILE
    AccessFsReadDir -> lANDLOCK_ACCESS_FS_READ_DIR
    AccessFsRemoveDir -> lANDLOCK_ACCESS_FS_REMOVE_DIR
    AccessFsRemoveFile -> lANDLOCK_ACCESS_FS_REMOVE_FILE
    AccessFsMakeChar -> lANDLOCK_ACCESS_FS_MAKE_CHAR
    AccessFsMakeDir -> lANDLOCK_ACCESS_FS_MAKE_DIR
    AccessFsMakeReg -> lANDLOCK_ACCESS_FS_MAKE_REG
    AccessFsMakeSock -> lANDLOCK_ACCESS_FS_MAKE_SOCK
    AccessFsMakeFifo -> lANDLOCK_ACCESS_FS_MAKE_FIFO
    AccessFsMakeBlock -> lANDLOCK_ACCESS_FS_MAKE_BLOCK
    AccessFsMakeSym -> lANDLOCK_ACCESS_FS_MAKE_SYM
    AccessFsRefer -> lANDLOCK_ACCESS_FS_REFER

-- | All 'AccessFsFlag' flags keyed by a Landlock ABI 'Version'.
accessFsFlags :: [(Version, [AccessFsFlag])]
accessFsFlags = [(v, [f | f <- [minBound..], version f <= v]) | v <- [
      version1
    , version2
    ]
  ]
  where
    version = \case
      AccessFsExecute -> version1
      AccessFsWriteFile -> version1
      AccessFsReadFile -> version1
      AccessFsReadDir -> version1
      AccessFsRemoveDir -> version1
      AccessFsRemoveFile -> version1
      AccessFsMakeChar -> version1
      AccessFsMakeDir -> version1
      AccessFsMakeReg -> version1
      AccessFsMakeSock -> version1
      AccessFsMakeFifo -> version1
      AccessFsMakeBlock -> version1
      AccessFsMakeSym -> version1
      AccessFsRefer -> version2

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
    AccessFsRefer -> False

-- | Flags passed to @landlock_create_ruleset@.
--
-- In the current kernel API, only @LANDLOCK_CREATE_RULESET_VERSION@ is
-- defined, which should not be used when creating an actual Landlock
-- encironment (cf. 'landlock').
data CreateRulesetFlag = CreateRulesetVersion
  deriving (Show, Eq, Enum, Bounded)

createRulesetFlagToBit :: CreateRulesetFlag -> U32
createRulesetFlagToBit = \case
    CreateRulesetVersion -> lANDLOCK_CREATE_RULESET_VERSION

-- | Flags passed to @landlock_restrict_self@.
--
-- In the current kernel API, no such flags are defined, hence this is a type
-- without any constructors.
data RestrictSelfFlag
  deriving (Show, Eq)

restrictSelfFlagToBit :: RestrictSelfFlag -> U32
restrictSelfFlagToBit = \case {}

-- | Flags passed to @landlock_add_rule@.
--
-- In the current kernel API, no such flags are defined, hence this is a type
-- without any constructors.
data AddRuleFlag
  deriving (Show, Eq)

addRuleFlagToBit :: AddRuleFlag -> U32
addRuleFlagToBit = \case {}
