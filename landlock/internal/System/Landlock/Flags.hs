{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE LambdaCase #-}

module System.Landlock.Flags
  ( toBits,
    fromBits,
    AccessFsFlag (..),
    accessFsFlagToBit,
    accessFsFlags,
    accessFsFlagIsReadOnly,
    AccessNetFlag (..),
    accessNetFlagToBit,
    accessNetFlags,
    CreateRulesetFlag (..),
    createRulesetFlagToBit,
    RestrictSelfFlag,
    restrictSelfFlagToBit,
    AddRuleFlag,
    addRuleFlagToBit,
  )
where

import Data.Bits (Bits, (.&.), (.|.))
import System.Landlock.Hsc
  ( U32,
    U64,
    lANDLOCK_ACCESS_FS_EXECUTE,
    lANDLOCK_ACCESS_FS_MAKE_BLOCK,
    lANDLOCK_ACCESS_FS_MAKE_CHAR,
    lANDLOCK_ACCESS_FS_MAKE_DIR,
    lANDLOCK_ACCESS_FS_MAKE_FIFO,
    lANDLOCK_ACCESS_FS_MAKE_REG,
    lANDLOCK_ACCESS_FS_MAKE_SOCK,
    lANDLOCK_ACCESS_FS_MAKE_SYM,
    lANDLOCK_ACCESS_FS_READ_DIR,
    lANDLOCK_ACCESS_FS_READ_FILE,
    lANDLOCK_ACCESS_FS_REFER,
    lANDLOCK_ACCESS_FS_REMOVE_DIR,
    lANDLOCK_ACCESS_FS_REMOVE_FILE,
    lANDLOCK_ACCESS_FS_TRUNCATE,
    lANDLOCK_ACCESS_FS_WRITE_FILE,
    lANDLOCK_ACCESS_NET_BIND_TCP,
    lANDLOCK_ACCESS_NET_CONNECT_TCP,
    lANDLOCK_CREATE_RULESET_VERSION,
  )
import System.Landlock.Version (Version, version1, version2, version3, version4)

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
-- - 'AccessFsTruncate'
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
-- actions acessible through these syscall families:
-- [@chdir@](https://man.archlinux.org/man/chdir.2),
-- [@stat@](https://man.archlinux.org/man/stat.2),
-- [@flock@](https://man.archlinux.org/man/flock.2),
-- [@chmod@](https://man.archlinux.org/man/chmod.2),
-- [@chown@](https://man.archlinux.org/man/chown.2),
-- [@setxattr@](https://man.archlinux.org/man/setxattr.2),
-- [@utime@](https://man.archlinux.org/man/utime.2),
-- [@ioctl@](https://man.archlinux.org/man/ioctl.2),
-- [@fcntl@](https://man.archlinux.org/man/fcntl.2),
-- [@access@](https://man.archlinux.org/man/access.2).
-- Future Landlock evolutions will enable to restrict them.
data AccessFsFlag
  = -- | Execute a file
    -- ([@LANDLOCK_ACCESS_FS_EXECUTE@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_EXECUTE)).
    AccessFsExecute
  | -- | Open a file with write access
    -- ([@LANDLOCK_ACCESS_FS_WRITE_FILE@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_WRITE_FILE)).
    --
    -- Note that you might additionally need the @LANDLOCK_ACCESS_FS_TRUNCATE@
    -- right in order to overwrite files with
    -- [@open@](https://man.archlinux.org/man/open.2) using
    -- [@O_TRUNC@](https://man.archlinux.org/man/open.2#O_TRUNC) or
    -- [@creat@](https://man.archlinux.org/man/creat.2).
    AccessFsWriteFile
  | -- | Open a file with read access
    -- ([@LANDLOCK_ACCESS_FS_READ_FILE@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_READ_FILE)).
    AccessFsReadFile
  | -- | Open a directory or list its content
    -- ([@LANDLOCK_ACCESS_FS_READ_DIR@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_READ_DIR)).
    AccessFsReadDir
  | -- | Remove an empty directory or rename one
    -- ([@LANDLOCK_ACCESS_FS_REMOVE_DIR@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_REMOVE_DIR)).
    AccessFsRemoveDir
  | -- | Unlink (or rename) a file
    -- ([@LANDLOCK_ACCESS_FS_REMOVE_FILE@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_REMOVE_FILE)).
    AccessFsRemoveFile
  | -- | Create (or rename or link) a character device
    -- ([@LANDLOCK_ACCESS_FS_MAKE_CHAR@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_MAKE_CHAR)).
    AccessFsMakeChar
  | -- | Create (or rename) a directory
    -- ([@LANDLOCK_ACCESS_FS_MAKE_DIR@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_MAKE_DIR)).
    AccessFsMakeDir
  | -- | Create (or rename or link) a regular file
    -- ([@LANDLOCK_ACCESS_FS_MAKE_REG@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_MAKE_REG)).
    AccessFsMakeReg
  | -- | Create (or rename or link) a UNIX domain socket
    -- ([@LANDLOCK_ACCESS_FS_MAKE_SOCK@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_MAKE_SOCK)).
    AccessFsMakeSock
  | -- | Create (or rename or link) a named pipe
    -- ([@LANDLOCK_ACCESS_FS_MAKE_FIFO@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_MAKE_FIFO)).
    AccessFsMakeFifo
  | -- | Create (or rename or link) a block device
    -- ([@LANDLOCK_ACCESS_FS_MAKE_BLOCK@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_MAKE_BLOCK)).
    AccessFsMakeBlock
  | -- | Create (or rename or link) a symbolic link
    -- ([@LANDLOCK_ACCESS_FS_MAKE_SYM@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_MAKE_SYM)).
    AccessFsMakeSym
  | -- | Link or rename a file from or to a different
    -- directory (i.e. reparent a file hierarchy)
    -- ([@LANDLOCK_ACCESS_FS_REFER@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_REFER)).
    --
    -- This access right is available since the second version of the Landlock
    -- ABI.
    --
    -- This is the only access right which is denied by default by any ruleset,
    -- even if the right is not specified as handled at ruleset creation time.
    -- The only way to make a ruleset grant this right is to explicitly allow it
    -- for a specific directory by adding a matching rule to the ruleset.
    --
    -- In particular, when using the first Landlock ABI version, Landlock will
    -- always deny attempts to reparent files between different directories.
    --
    -- In addition to the source and destination directories having the
    -- @LANDLOCK_ACCESS_FS_REFER@ access right, the attempted link or rename
    -- operation must meet the following constraints:
    --
    -- * The reparented file may not gain more access rights in the destination
    --   directory than it previously had in the source directory.  If this is
    --   attempted, the operation results in an [@EXDEV@](https://man.archlinux.org/man/errno.3#EXDEV) error.
    --
    -- * When linking or renaming, the @LANDLOCK_ACCESS_FS_MAKE_*@ right for the
    --   respective file type must be granted for the destination directory.
    --   Otherwise, the operation results in an [@EACCES@](https://man.archlinux.org/man/errno.3#EACCES) error.
    --
    -- * When renaming, the @LANDLOCK_ACCESS_FS_REMOVE_*@ right for the
    --   respective file type must be granted for the source directory.  Otherwise,
    --   the operation results in an [@EACCES@](https://man.archlinux.org/man/errno.3#EACCES) error.
    --
    -- If multiple requirements are not met, the [@EACCES@](https://man.archlinux.org/man/errno.3#EACCES) error code takes
    -- precedence over [@EXDEV@](https://man.archlinux.org/man/errno.3#EXDEV).
    AccessFsRefer
  | -- | Truncate a file with
    -- [@truncate@](https://man.archlinux.org/man/truncate.2),
    -- [@ftruncate@](https://man.archlinux.org/man/ftruncate.2),
    -- [@creat@](https://man.archlinux.org/man/creat.2), or
    -- [@open@](https://man.archlinux.org/man/open.2) with
    -- [@O_TRUNC@](https://man.archlinux.org/man/open.2#O_TRUNC). Whether an
    -- opened file can be truncated with
    -- [@ftruncate@](https://man.archlinux.org/man/ftruncate.2) is determined
    -- during @open@, in the same way as read and write permissions are checked
    -- during [@open@](https://man.archlinux.org/man/open.2) using
    -- [@LANDLOCK_ACCESS_FS_READ_FILE@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_READ_FILE)
    -- and
    -- [@LANDLOCK_ACCESS_FS_WRITE_FILE@](https://man.archlinux.org/man/landlock.7.en#LANDLOCK_ACCESS_FS_WRITE_FILE).
    -- This access right is available since the third version of the Landlock ABI.
    AccessFsTruncate
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
  AccessFsTruncate -> lANDLOCK_ACCESS_FS_TRUNCATE

-- | All 'AccessFsFlag' flags keyed by a Landlock ABI 'Version'.
accessFsFlags :: [(Version, [AccessFsFlag])]
accessFsFlags =
  [ (v, [f | f <- [minBound ..], version f <= v])
    | v <-
        [ version1,
          version2,
          version3,
          version4
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
      AccessFsTruncate -> version3

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
  AccessFsTruncate -> False

data AccessNetFlag
  = AccessNetBindTcp
  | AccessNetConnectTcp
  deriving (Show, Eq, Enum, Bounded, Ord)

accessNetFlagToBit :: AccessNetFlag -> U64
accessNetFlagToBit = \case
  AccessNetBindTcp -> lANDLOCK_ACCESS_NET_BIND_TCP
  AccessNetConnectTcp -> lANDLOCK_ACCESS_NET_CONNECT_TCP

accessNetFlags :: [(Version, [AccessNetFlag])]
accessNetFlags =
  [ (v, [f | f <- [minBound ..], version f <= v])
    | v <-
        [ version1,
          version2,
          version3,
          version4
        ]
  ]
  where
    version = \case
      AccessNetBindTcp -> version4
      AccessNetConnectTcp -> version4

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
