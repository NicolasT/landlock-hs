module ReadmeUtils
  ( withFakeRoot,
    -- Control.Exception.Base
    handleJust,
    -- Control.Monad
    unless,
    -- Data.List
    sort,
    -- Data.Maybe
    listToMaybe,
    -- System.Directory
    removeFile,
    -- System.FilePath
    (</>),
    -- System.IO
    IOMode (..),
    hPutStrLn,
    withFile,
    writeFile,
    -- System.IO.Error
    isPermissionError,
    -- System.Process
    readProcess,
  )
where

import Control.Exception.Base (handleJust)
import Control.Monad (unless)
import Data.List (sort)
import Data.Maybe (listToMaybe)
import System.Directory (createDirectory, removeFile)
import System.FilePath ((</>))
import System.IO (IOMode (..), hPutStrLn, withFile)
import System.IO.Error (isPermissionError)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)

withFakeRoot :: (FilePath -> IO a) -> IO a
withFakeRoot fn = withSystemTempDirectory "landlock-readme" $ \tmp -> do
  let homeDir = tmp </> "home"
      userDir = homeDir </> "user"
      sshDir = userDir </> ".ssh"
      tmpDir = tmp </> "tmp"

  mapM_
    createDirectory
    [ homeDir,
      userDir,
      sshDir,
      tmpDir
    ]

  let privateKey = sshDir </> "id_ed25519"
      publicKey = sshDir </> "id_ed25519.pub"

  writeFile privateKey "private"
  writeFile publicKey "public"

  fn tmp
