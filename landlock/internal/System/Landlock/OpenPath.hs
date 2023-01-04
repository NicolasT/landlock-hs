{-# LANGUAGE CApiFFI #-}

module System.Landlock.OpenPath (
      withOpenPath
    , withOpenPathAt
    , OpenPathFlags(..)
    , defaultOpenPathFlags
    ) where

import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..))
import System.Posix.Error (throwErrnoPathIfMinus1Retry)
import System.Posix.IO (closeFd)
import System.Posix.Types (Fd)

import System.Landlock.Hsc (
    aT_FDCWD, o_CLOEXEC, o_DIRECTORY, o_NOFOLLOW, o_PATH, o_RDONLY)

foreign import capi unsafe "fcntl.h openat"
  _openat :: CInt
         -> CString
         -> CInt
         -> IO CInt

openat :: Fd
       -> FilePath
       -> CInt
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
withOpenPath = withOpenPathAt (fromIntegral aT_FDCWD)

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
    flags' = foldr (.|.) 0 [ o_PATH
                           , o_RDONLY
                           , if directory flags then o_DIRECTORY else 0
                           , if nofollow flags then o_NOFOLLOW else 0
                           , if cloexec flags then o_CLOEXEC else 0
                           ]
