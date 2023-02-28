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

-- | Extra flags used by 'withOpenPathAt' in the call to
-- [@openat@](https://man.archlinux.org/man/openat.2).
data OpenPathFlags = OpenPathFlags
  { -- | Set [@O_DIRECTORY@](https://man.archlinux.org/man/openat.2#O_DIRECTORY).
    directory :: Bool,
    -- | Set [@O_NOFOLLOW@](https://man.archlinux.org/man/openat.2#O_NOFOLLOW).
    nofollow :: Bool,
    -- | Set [@O_CLOEXEC@](https://man.archlinux.org/man/openat.2#O_CLOEXEC).
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

-- | Perform an action with a path opened using
-- [@O_PATH@](https://man.archlinux.org/man/openat.2#O_PATH).
--
-- The file descriptor provided to the action will be
-- [@close@](https://man.archlinux.org/man/close.2)d when the function returns.
--
-- This internally calls [@openat@](https://man.archlinux.org/man/openat.2) with
-- @AT_FDCWD@ and the [@O_PATH@](https://man.archlinux.org/man/openat.2#O_PATH)
-- and [@O_RDONLY@](https://man.archlinux.org/man/open.2#File_access_mode) flags
-- set, next to any flags specified in the 'OpenPathFlags' argument.
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

-- | Perform an action with a path
-- [@openat@](https://man.archlinux.org/man/openat.2)ed using
-- [@O_PATH@](https://man.archlinux.org/man/openat.2#O_PATH).
--
-- Like 'withOpenPath', exposing the
-- [@openat@](https://man.archlinux.org/man/openat.2#O_PATH) @dirfd@ argument.
--
-- The file descriptor provided to the action will be
-- [@close@](https://man.archlinux.org/man/close.2)d when the
-- function returns.
--
-- This internally calls
-- [@openat@](https://man.archlinux.org/man/openat.2) with the
-- [@O_PATH@](https://man.archlinux.org/man/openat.2#O_PATH) and
-- [@O_RDONLY@](https://man.archlinux.org/man/openat.2#File_access_mode) flags
-- set, next to any flags specified in the 'OpenPathFlags' argument.
withOpenPathAt ::
  (MonadIO m, MonadMask m) =>
  -- | @dirfd@ argument to [@openat@](https://man.archlinux.org/man/openat.2).
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
