{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module System.Landlock.Rules (
      Rule
    , RuleType(..)
    , ruleType
    , pathBeneath
    , AccessFsFlag(..)
    ) where

#include <sys/types.h>

#include "linux/landlock.h"

import Data.Int (Int32)
import Data.Word (Word32, Word64)
import Foreign.Storable (Storable(..))
import System.Posix.Types (Fd)

import System.Landlock.Flags (AccessFsFlag(..), accessFsFlagToBit, fromBits, toBits)

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
ruleType :: Rule a -> #{type enum landlock_rule_type}
ruleType = \case
    PathBeneathRule{} -> #{const LANDLOCK_RULE_PATH_BENEATH}

instance Storable (Rule 'PathBeneath) where
    sizeOf _ = #{size struct landlock_path_beneath_attr}
    alignment _ = #{alignment struct landlock_path_beneath_attr}
    peek ptr = do
        allowedAccess <- #{peek struct landlock_path_beneath_attr, allowed_access} ptr :: IO #{type __u64}
        parentFd <- #{peek struct landlock_path_beneath_attr, parent_fd} ptr :: IO #{type __s32}
        return $ PathBeneathRule (fromBits allowedAccess accessFsFlagToBit) (fromIntegral parentFd)
    poke ptr (PathBeneathRule flags fd) = do
        let allowedAccess = toBits flags accessFsFlagToBit :: #{type __u64}
            parentFd = fromIntegral fd :: #{type __s32}
        #{poke struct landlock_path_beneath_attr, allowed_access} ptr allowedAccess
        #{poke struct landlock_path_beneath_attr, parent_fd} ptr parentFd

-- | Construct a path hierarchy rule definition.
--
-- This corresponds to a rule of type @LANDLOCK_RULE_PATH_BENEATH@, with
-- attributes defined in a @struct landlock_path_beneath_attr@.
pathBeneath :: Fd  -- ^ File descriptor, preferably opened with @O_PATH@, which
                   --   identifies the parent directory of a file hierarchy, or
                   --   just a file.
            -> [AccessFsFlag]  -- ^ Allowed actions for this file hierarchy
                               --   (cf. 'AccessFsFlag').
            -> Rule 'PathBeneath
pathBeneath fd flags = PathBeneathRule flags fd
