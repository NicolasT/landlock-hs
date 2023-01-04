{-# LANGUAGE CApiFFI #-}

module System.Landlock.OpenPath
  ( withOpenPath,
    withOpenPathAt,
    OpenPathFlags (..),
    defaultOpenPathFlags,
  )
where

import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import System.Landlock.Hsc
  ( aT_FDCWD,
    o_CLOEXEC,
    o_DIRECTORY,
    o_NOFOLLOW,
    o_PATH,
    o_RDONLY,
  )
import System.Posix.Error (throwErrnoPathIfMinus1Retry)
import System.Posix.IO (closeFd)
import System.Posix.Types (Fd)

foreign import capi unsafe "fcntl.h openat"
  _openat ::
    CInt ->
    CString ->
    CInt ->
    IO CInt

openat ::
  Fd ->
  FilePath ->
  CInt ->
  IO Fd
openat dirfd pathname flags = withCString pathname $ \pathnamep -> do
  throwErrnoPathIfMinus1Retry "openat" pathname $
    fromIntegral <$> _openat (fromIntegral dirfd) pathnamep flags

-- | Extra flags used by 'withOpenPathAt' in the call to @openat@.
data OpenPathFlags = OpenPathFlags
  { -- | Set @O_DIRECTORY@.
    directory :: Bool,
    -- | Set @O_NOFOLLOW@.
    nofollow :: Bool,
    -- | Set @O_CLOEXEC@.
    cloexec :: Bool
  }
  deriving (Show, Eq)

-- | Default 'OpenPathFlags':
--
-- - 'directory' is @False@.
-- - 'nofollow' is @False@.
-- - 'cloexec' is @True@.
defaultOpenPathFlags :: OpenPathFlags
defaultOpenPathFlags =
  OpenPathFlags
    { directory = False,
      nofollow = False,
      cloexec = True
    }

-- | Perform an action with a path @open@ed using @O_PATH@.
--
-- The file descriptor provided to the action will be @close@d when the
-- function returns.
--
-- This internally calls @openat@ with @AT_FDCWD@ and the @O_PATH@ and
-- @O_RDONLY@ flags set, next to any flags specified in the 'OpenPathFlags'
-- argument.
withOpenPath ::
  (MonadIO m, MonadMask m) =>
  -- | Path to open.
  FilePath ->
  -- | Flag settings to pass.
  OpenPathFlags ->
  -- | Action to call with a file descriptor to the given path.
  (Fd -> m a) ->
  -- | Result of the invoked action.
  m a
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
withOpenPathAt ::
  (MonadIO m, MonadMask m) =>
  -- | @dirfd@ argument to @openat@.
  Fd ->
  -- | Path to open.
  FilePath ->
  -- | Flag settings to pass.
  OpenPathFlags ->
  -- | Action to call with a file descriptor to the given path.
  (Fd -> m a) ->
  -- | Result of the invoked action.
  m a
withOpenPathAt dirfd pathname flags =
  bracket
    (liftIO $ openat dirfd pathname flags')
    (liftIO . closeFd)
  where
    flags' =
      foldr
        (.|.)
        0
        [ o_PATH,
          o_RDONLY,
          if directory flags then o_DIRECTORY else 0,
          if nofollow flags then o_NOFOLLOW else 0,
          if cloexec flags then o_CLOEXEC else 0
        ]
