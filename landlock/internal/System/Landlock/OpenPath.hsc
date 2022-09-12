{-# LANGUAGE CApiFFI #-}

module System.Landlock.OpenPath (
      withOpenPath
    , withOpenPathAt
    , OpenPathFlags(..)
    , defaultOpenPathFlags
    ) where

#define _GNU_SOURCE

#include <fcntl.h>

import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))
import Data.Int (Int32)
import Foreign.C.String (CString, withCString)
import System.Posix.Error (throwErrnoPathIfMinus1Retry)
import System.Posix.IO (closeFd)
import System.Posix.Types (Fd)

foreign import capi unsafe "fcntl.h openat"
  _openat :: #{type int}
         -> CString
         -> #{type int}
         -> IO #{type int}

openat :: Fd
       -> FilePath
       -> #{type int}
       -> IO Fd
openat dirfd pathname flags = withCString pathname $ \pathnamep -> do
    fd <- throwErrnoPathIfMinus1Retry "openat" pathname $ _openat (fromIntegral dirfd) pathnamep flags
    return $ fromIntegral fd

-- | Extra flags used by 'withOpenPathAt' in the call to @openat@.
data OpenPathFlags = OpenPathFlags { directory :: Bool
                                     -- ^ Set @O_DIRECTORY@.
                                   , nofollow :: Bool
                                     -- ^ Set @O_NOFOLLOW@.
                                   , cloexec :: Bool
                                     -- ^ Set @O_CLOEXEC@.
                                   }
  deriving (Show, Eq)

-- | Default 'OpenPathFlags':
--
-- - 'directory' is @False@.
-- - 'nofollow' is @False@.
-- - 'cloexec' is @True@.
defaultOpenPathFlags :: OpenPathFlags
defaultOpenPathFlags = OpenPathFlags { directory = False
                                     , nofollow = False
                                     , cloexec = True
                                     }

-- | Perform an action with a path @open@ed using @O_PATH@.
--
-- The file descriptor provided to the action will be @close@d when the
-- function returns.
--
-- This internally calls @openat@ with @AT_FDCWD@ and the @O_PATH@ and
-- @O_RDONLY@ flags set, next to any flags specified in the 'OpenPathFlags'
-- argument.
withOpenPath :: (MonadIO m, MonadMask m)
             => FilePath  -- ^ Path to open.
             -> OpenPathFlags  -- ^ Flag settings to pass.
             -> (Fd -> m a)  -- ^ Action to call with a file descriptor to the given path.
             -> m a  -- ^ Result of the invoked action.
withOpenPath = withOpenPathAt (fromIntegral (#{const AT_FDCWD} :: #{type int}))

-- | Perform an action with a path @openat@ed using @O_PATH@.
--
-- Like 'withOpenPath', exposing the @openat@ @dirfd@ argument.
--
-- The file descriptor provided to the action will be @close@d when the
-- function returns.
--
-- This internally calls @openat@ with the @O_PATH@ and @O_RDONLY@ flags set,
-- next to any flags specified in the 'OpenPathFlags' argument.
withOpenPathAt :: (MonadIO m, MonadMask m)
               => Fd  -- ^ @dirfd@ argument to @openat@.
               -> FilePath  -- ^ Path to open.
               -> OpenPathFlags  -- ^ Flag settings to pass.
               -> (Fd -> m a)  -- ^ Action to call with a file descriptor to the given path.
               -> m a  -- ^ Result of the invoked action.
withOpenPathAt dirfd pathname flags = bracket (liftIO $ openat dirfd pathname flags') (liftIO . closeFd)
  where
    flags' = foldr (.|.) 0 [ #{const O_PATH}
                           , #{const O_RDONLY}
                           , if directory flags then #{const O_DIRECTORY} else 0
                           , if nofollow flags then #{const O_NOFOLLOW} else 0
                           , if cloexec flags then #{const O_CLOEXEC} else 0
                           ]
