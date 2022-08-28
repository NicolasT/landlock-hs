module System.Landlock.Version (
      Version(..)
    , version1
    , version2
    ) where

import Data.Int (Int64)

-- | Representation of a Landlock ABI version as reported by the kernel.
newtype Version = Version { getVersion :: #{type long} }
  deriving (Show, Eq, Ord)

-- All ABI versions supported by this library should be exposed as a value.
-- | ABI version 1.
version1 :: Version
version1 = Version 1

-- | ABI version 2.
version2 :: Version
version2 = Version 2
