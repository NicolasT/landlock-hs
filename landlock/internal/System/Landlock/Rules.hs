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
    pathBeneath,
    AccessFsFlag (..),
  )
where

import Foreign.Storable (Storable (..))
import System.Landlock.Flags
  ( AccessFsFlag (..),
    accessFsFlagToBit,
    fromBits,
    toBits,
  )
import System.Landlock.Hsc
  ( Landlock_rule_type,
    lANDLOCK_RULE_PATH_BENEATH,
    landlock_path_beneath_attr_alignment,
    landlock_path_beneath_attr_peek_allowed_access,
    landlock_path_beneath_attr_peek_parent_fd,
    landlock_path_beneath_attr_poke_allowed_access,
    landlock_path_beneath_attr_poke_parent_fd,
    landlock_path_beneath_attr_size,
  )
import System.Posix.Types (Fd)

-- | Kind of 'Rule's.
data RuleType = PathBeneath

-- | A rule enforced by Landlock, to be registered using 'System.Landlock.addRule'.
--
-- 'Rule's can be constructed using the relevant functions, like 'pathBeneath'.
data Rule (a :: RuleType) where
  PathBeneathRule :: [AccessFsFlag] -> Fd -> Rule 'PathBeneath

deriving instance Show (Rule a)

deriving instance Eq (Rule a)

-- | Retrieve the @enum landlock_rule_type@ value of a given 'Rule'.
ruleType :: Rule a -> Landlock_rule_type
ruleType = \case
  PathBeneathRule {} -> lANDLOCK_RULE_PATH_BENEATH

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

-- | Construct a path hierarchy rule definition.
--
-- This corresponds to a rule of type @LANDLOCK_RULE_PATH_BENEATH@, with
-- attributes defined in a @struct landlock_path_beneath_attr@.
pathBeneath ::
  -- | File descriptor, preferably opened with @O_PATH@, which
  --   identifies the parent directory of a file hierarchy, or
  --   just a file.
  Fd ->
  -- | Allowed actions for this file hierarchy
  --   (cf. 'AccessFsFlag').
  [AccessFsFlag] ->
  Rule 'PathBeneath
pathBeneath fd flags = PathBeneathRule flags fd
