module System.Landlock.Hsc (
  aT_FDCWD,

  o_PATH,
  o_RDONLY,
  o_DIRECTORY,
  o_NOFOLLOW,
  o_CLOEXEC,

  lANDLOCK_ACCESS_FS_EXECUTE,
  lANDLOCK_ACCESS_FS_WRITE_FILE,
  lANDLOCK_ACCESS_FS_READ_FILE,
  lANDLOCK_ACCESS_FS_READ_DIR,
  lANDLOCK_ACCESS_FS_REMOVE_DIR,
  lANDLOCK_ACCESS_FS_REMOVE_FILE,
  lANDLOCK_ACCESS_FS_MAKE_CHAR,
  lANDLOCK_ACCESS_FS_MAKE_DIR,
  lANDLOCK_ACCESS_FS_MAKE_REG,
  lANDLOCK_ACCESS_FS_MAKE_SOCK,
  lANDLOCK_ACCESS_FS_MAKE_FIFO,
  lANDLOCK_ACCESS_FS_MAKE_BLOCK,
  lANDLOCK_ACCESS_FS_MAKE_SYM,
  lANDLOCK_ACCESS_FS_REFER,
  lANDLOCK_ACCESS_FS_TRUNCATE,

  lANDLOCK_CREATE_RULESET_VERSION,

  U32,
  U64,
  Landlock_rule_type,

  pR_SET_NO_NEW_PRIVS,

  landlock_ruleset_attr_size,
  landlock_ruleset_attr_alignment,
  landlock_ruleset_attr_peek_handled_access_fs,
  landlock_ruleset_attr_poke_handled_access_fs,

  lANDLOCK_RULE_PATH_BENEATH,
  landlock_path_beneath_attr_size,
  landlock_path_beneath_attr_alignment,
  landlock_path_beneath_attr_peek_allowed_access,
  landlock_path_beneath_attr_poke_allowed_access,
  landlock_path_beneath_attr_peek_parent_fd,
  landlock_path_beneath_attr_poke_parent_fd,
) where

#define _GNU_SOURCE

#include <fcntl.h>
#include <stddef.h>
#include <sys/prctl.h>
#include <sys/types.h>

#include "hs-landlock.h"
#include "linux/landlock.h"

import Data.Int (Int32)
import Data.Word (Word32, Word64)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff, pokeByteOff)

aT_FDCWD :: CInt
aT_FDCWD = #{const AT_FDCWD}

o_PATH :: CInt
o_PATH = #{const O_PATH}

o_RDONLY :: CInt
o_RDONLY = #{const O_RDONLY}

o_DIRECTORY :: CInt
o_DIRECTORY = #{const O_DIRECTORY}

o_NOFOLLOW :: CInt
o_NOFOLLOW = #{const O_NOFOLLOW}

o_CLOEXEC :: CInt
o_CLOEXEC = #{const O_CLOEXEC}

lANDLOCK_ACCESS_FS_EXECUTE :: #{type __u64}
lANDLOCK_ACCESS_FS_EXECUTE = #{const LANDLOCK_ACCESS_FS_EXECUTE}

lANDLOCK_ACCESS_FS_WRITE_FILE :: #{type __u64}
lANDLOCK_ACCESS_FS_WRITE_FILE = #{const LANDLOCK_ACCESS_FS_WRITE_FILE}

lANDLOCK_ACCESS_FS_READ_FILE :: #{type __u64}
lANDLOCK_ACCESS_FS_READ_FILE = #{const LANDLOCK_ACCESS_FS_READ_FILE}

lANDLOCK_ACCESS_FS_READ_DIR :: #{type __u64}
lANDLOCK_ACCESS_FS_READ_DIR = #{const LANDLOCK_ACCESS_FS_READ_DIR}

lANDLOCK_ACCESS_FS_REMOVE_DIR :: #{type __u64}
lANDLOCK_ACCESS_FS_REMOVE_DIR = #{const LANDLOCK_ACCESS_FS_REMOVE_DIR}

lANDLOCK_ACCESS_FS_REMOVE_FILE :: #{type __u64}
lANDLOCK_ACCESS_FS_REMOVE_FILE = #{const LANDLOCK_ACCESS_FS_REMOVE_FILE}

lANDLOCK_ACCESS_FS_MAKE_CHAR :: #{type __u64}
lANDLOCK_ACCESS_FS_MAKE_CHAR = #{const LANDLOCK_ACCESS_FS_MAKE_CHAR}

lANDLOCK_ACCESS_FS_MAKE_DIR :: #{type __u64}
lANDLOCK_ACCESS_FS_MAKE_DIR = #{const LANDLOCK_ACCESS_FS_MAKE_DIR}

lANDLOCK_ACCESS_FS_MAKE_REG :: #{type __u64}
lANDLOCK_ACCESS_FS_MAKE_REG = #{const LANDLOCK_ACCESS_FS_MAKE_REG}

lANDLOCK_ACCESS_FS_MAKE_SOCK :: #{type __u64}
lANDLOCK_ACCESS_FS_MAKE_SOCK = #{const LANDLOCK_ACCESS_FS_MAKE_SOCK}

lANDLOCK_ACCESS_FS_MAKE_FIFO :: #{type __u64}
lANDLOCK_ACCESS_FS_MAKE_FIFO = #{const LANDLOCK_ACCESS_FS_MAKE_FIFO}

lANDLOCK_ACCESS_FS_MAKE_BLOCK :: #{type __u64}
lANDLOCK_ACCESS_FS_MAKE_BLOCK = #{const LANDLOCK_ACCESS_FS_MAKE_BLOCK}

lANDLOCK_ACCESS_FS_MAKE_SYM :: #{type __u64}
lANDLOCK_ACCESS_FS_MAKE_SYM = #{const LANDLOCK_ACCESS_FS_MAKE_SYM}

lANDLOCK_ACCESS_FS_REFER :: #{type __u64}
lANDLOCK_ACCESS_FS_REFER = #{const LANDLOCK_ACCESS_FS_REFER}

lANDLOCK_ACCESS_FS_TRUNCATE :: #{type __u64}
lANDLOCK_ACCESS_FS_TRUNCATE = #{const LANDLOCK_ACCESS_FS_TRUNCATE}

lANDLOCK_CREATE_RULESET_VERSION :: #{type __u32}
lANDLOCK_CREATE_RULESET_VERSION = #{const LANDLOCK_CREATE_RULESET_VERSION}

type U32 = #{type __u32}
type U64 = #{type __u64}

type Landlock_rule_type = #{type enum landlock_rule_type}

pR_SET_NO_NEW_PRIVS :: CInt
pR_SET_NO_NEW_PRIVS = #{const PR_SET_NO_NEW_PRIVS}

landlock_ruleset_attr_size :: Int
landlock_ruleset_attr_size = #{size struct landlock_ruleset_attr}

landlock_ruleset_attr_alignment :: Int
landlock_ruleset_attr_alignment = #{alignment struct landlock_ruleset_attr}

landlock_ruleset_attr_peek_handled_access_fs :: Ptr a -> IO #{type __u64}
landlock_ruleset_attr_peek_handled_access_fs =
  #{peek struct landlock_ruleset_attr, handled_access_fs}

landlock_ruleset_attr_poke_handled_access_fs :: Ptr a -> #{type __u64} -> IO ()
landlock_ruleset_attr_poke_handled_access_fs =
  #{poke struct landlock_ruleset_attr, handled_access_fs}

lANDLOCK_RULE_PATH_BENEATH :: #{type enum landlock_rule_type}
lANDLOCK_RULE_PATH_BENEATH = #{const LANDLOCK_RULE_PATH_BENEATH}

landlock_path_beneath_attr_size :: Int
landlock_path_beneath_attr_size = #{size struct landlock_path_beneath_attr}

landlock_path_beneath_attr_alignment :: Int
landlock_path_beneath_attr_alignment =
  #{alignment struct landlock_path_beneath_attr}

landlock_path_beneath_attr_peek_allowed_access :: Ptr a -> IO #{type __u64}
landlock_path_beneath_attr_peek_allowed_access =
  #{peek struct landlock_path_beneath_attr, allowed_access}

landlock_path_beneath_attr_peek_parent_fd :: Ptr a -> IO #{type __s32}
landlock_path_beneath_attr_peek_parent_fd =
  #{peek struct landlock_path_beneath_attr, parent_fd}

landlock_path_beneath_attr_poke_allowed_access ::
  Ptr a -> #{type __u64} -> IO ()
landlock_path_beneath_attr_poke_allowed_access =
  #{poke struct landlock_path_beneath_attr, allowed_access}

landlock_path_beneath_attr_poke_parent_fd :: Ptr a -> #{type __s32} -> IO ()
landlock_path_beneath_attr_poke_parent_fd =
  #{poke struct landlock_path_beneath_attr, parent_fd}


