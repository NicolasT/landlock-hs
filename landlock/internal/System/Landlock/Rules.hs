{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module System.Landlock.Rules
  ( Rule,
    RuleType (..),
    ruleType,
    netPort,
    AccessNetFlag (..),
    pathBeneath,
    AccessFsFlag (..),
  )
where

import Foreign.Storable (Storable (..))
import Network.Socket (PortNumber)
import System.Landlock.Flags
  ( AccessFsFlag (..),
    AccessNetFlag (..),
    accessFsFlagToBit,
    accessNetFlagToBit,
    fromBits,
    toBits,
  )
import System.Landlock.Hsc
  ( Landlock_rule_type,
    lANDLOCK_RULE_NET_PORT,
    lANDLOCK_RULE_PATH_BENEATH,
    landlock_net_port_attr_alignment,
    landlock_net_port_attr_peek_allowed_access,
    landlock_net_port_attr_peek_port,
    landlock_net_port_attr_poke_allowed_access,
    landlock_net_port_attr_poke_port,
    landlock_net_port_attr_size,
    landlock_path_beneath_attr_alignment,
    landlock_path_beneath_attr_peek_allowed_access,
    landlock_path_beneath_attr_peek_parent_fd,
    landlock_path_beneath_attr_poke_allowed_access,
    landlock_path_beneath_attr_poke_parent_fd,
    landlock_path_beneath_attr_size,
  )
import System.Posix.Types (Fd)

-- | Kind of 'Rule's.
data RuleType = PathBeneath | NetPort

-- | A rule enforced by Landlock, to be registered using 'System.Landlock.addRule'.
--
-- 'Rule's can be constructed using the relevant functions, like 'pathBeneath'.
data Rule (a :: RuleType) where
  PathBeneathRule :: [AccessFsFlag] -> Fd -> Rule 'PathBeneath
  NetPortRule :: [AccessNetFlag] -> PortNumber -> Rule 'NetPort

deriving instance Show (Rule a)

deriving instance Eq (Rule a)

-- | Retrieve the @enum landlock_rule_type@ value of a given 'Rule'.
ruleType :: Rule a -> Landlock_rule_type
ruleType = \case
  PathBeneathRule {} -> lANDLOCK_RULE_PATH_BENEATH
  NetPortRule {} -> lANDLOCK_RULE_NET_PORT

instance Storable (Rule 'PathBeneath) where
  sizeOf _ = landlock_path_beneath_attr_size
  alignment _ = landlock_path_beneath_attr_alignment
  peek ptr = do
    allowedAccess <- landlock_path_beneath_attr_peek_allowed_access ptr
    parentFd <- landlock_path_beneath_attr_peek_parent_fd ptr
    return $
      PathBeneathRule
        (fromBits accessFsFlagToBit allowedAccess)
        (fromIntegral parentFd)
  poke ptr (PathBeneathRule flags fd) = do
    let allowedAccess = toBits accessFsFlagToBit flags
        parentFd = fromIntegral fd
    landlock_path_beneath_attr_poke_allowed_access ptr allowedAccess
    landlock_path_beneath_attr_poke_parent_fd ptr parentFd

instance Storable (Rule 'NetPort) where
  sizeOf _ = landlock_net_port_attr_size
  alignment _ = landlock_net_port_attr_alignment
  peek ptr = do
    allowedAccess <- landlock_net_port_attr_peek_allowed_access ptr
    port <- landlock_net_port_attr_peek_port ptr
    return $
      NetPortRule
        (fromBits accessNetFlagToBit allowedAccess)
        (fromIntegral port)
  poke ptr (NetPortRule flags port) = do
    let allowedAccess = toBits accessNetFlagToBit flags
        port' = fromIntegral port
    landlock_net_port_attr_poke_allowed_access ptr allowedAccess
    landlock_net_port_attr_poke_port ptr port'

-- | Construct a path hierarchy rule definition.
--
-- This corresponds to a rule of type
-- [@LANDLOCK_RULE_PATH_BENEATH@](https://man.archlinux.org/man/landlock_add_rule.2.en#LANDLOCK_RULE_PATH_BENEATH),
-- with attributes defined in a @struct landlock_path_beneath_attr@.
pathBeneath ::
  -- | File descriptor, preferably opened with
  -- [@O_PATH@](https://man.archlinux.org/man/open.2#O_PATH), which
  -- identifies the parent directory of a file hierarchy, or
  -- just a file.
  Fd ->
  -- | Allowed actions for this file hierarchy
  --   (cf. 'AccessFsFlag').
  [AccessFsFlag] ->
  Rule 'PathBeneath
pathBeneath fd flags = PathBeneathRule flags fd

netPort :: PortNumber -> [AccessNetFlag] -> Rule 'NetPort
netPort port flags = NetPortRule flags port
