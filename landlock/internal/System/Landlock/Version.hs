module System.Landlock.Version
  ( Version (..),
    version1,
    version2,
    version3,
    version4,
  )
where

-- | Representation of a Landlock ABI version as reported by the kernel.
newtype Version = Version
  { -- | Get the numerical version.
    getVersion :: Word
  }
  deriving (Show, Eq, Ord)

-- All ABI versions supported by this library should be exposed as a value.

-- | ABI version 1.
version1 :: Version
version1 = Version 1

-- | ABI version 2.
version2 :: Version
version2 = Version 2

-- | ABI version 3.
version3 :: Version
version3 = Version 3

-- | ABI version 4.
version4 :: Version
version4 = Version 4
