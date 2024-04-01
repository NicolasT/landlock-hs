# landlock-hs: Haskell bindings for the Linux Landlock API

This library exposes Haskell bindings for the Linux kernel Landlock API.

The Linux kernel Landlock API provides unprivileged access control. The goal
of Landlock is to enable to restrict ambient rights (e.g. global filesystem
access) for a set of processes. Because Landlock is a stackable LSM, it makes
possible to create safe security sandboxes as new security layers in addition
to the existing system-wide access-controls. This kind of sandbox is expected
to help mitigate the security impact of bugs or unexpected/malicious
behaviors in user space applications. Landlock empowers any process,
including unprivileged ones, to securely restrict themselves.

For more information, see the [Landlock homepage](https://landlock.io/) and its
[kernel documentation](https://docs.kernel.org/userspace-api/landlock.html).

## Example

Here's a simple example, allowing read-only access to the user's SSH public
key, full access to `tmp` and the ability to read and execute everything in
`/usr`:

```haskell
import System.Landlock (
      AccessFsFlag(..)
    , OpenPathFlags(..)
    , RulesetAttr(..)
    , abiVersion
    , accessFsFlags
    , accessFsFlagIsReadOnly
    , accessNetFlags
    , defaultOpenPathFlags
    , isSupported
    , landlock
    , pathBeneath
    , withOpenPath
    )

import ReadmeUtils

-- These tests run with a "fake" pre-populated rootfs
-- All files and directories in it are created using the current user,
-- readable, writable and (in case of scripts) executable, so without
-- Landlock, there would be no permission errors.

main :: IO ()
main = withFakeRoot $ \root -> do
    -- Check whether Landlock is supported
    hasLandlock <- isSupported

    unless hasLandlock $
        fail "Landlock not supported"

    version <- abiVersion
    -- Find all FS access flags for the running kernel's ABI version
    allFsFlags <- lookupAccessFsFlags version
    let roFlags = filter accessFsFlagIsReadOnly allFsFlags
    -- Find all Net access flags for the running kernel's ABI version
    allNetFlags <- lookupAccessNetFlags version

    let homeDir = root </> "home" </> "user"
        publicKey = homeDir </> ".ssh" </> "id_ed25519.pub"
        privateKey = homeDir </> ".ssh" </> "id_ed25519"
        tmpDir = root </> "tmp"

    -- Construct the sandbox, locking down everything by default
    landlock (RulesetAttr allFsFlags allNetFlags) [] [] $ \addRule -> do
        -- /tmp is fully accessible
        withOpenPath tmpDir defaultOpenPathFlags{ directory = True } $ \tmp ->
            addRule (pathBeneath tmp allFsFlags) []

        -- SSH public key is readable
        withOpenPath publicKey defaultOpenPathFlags $ \key ->
            addRule (pathBeneath key [AccessFsReadFile]) []

        -- (Real) /usr is fully accessible, but read-only (includes executable permissions)
        withOpenPath "/usr" defaultOpenPathFlags{ directory = True } $ \usr -> do
            addRule (pathBeneath usr roFlags) []

    -- Can create and write to files in tmp
    writeFile (tmpDir </> "program.log") "Success!"

    -- Can read SSH key
    _pubKey <- readFile publicKey

    -- Can execute things in /usr
    "Linux\n" <- readProcess ("/usr" </> "bin" </> "uname") ["-s"] ""

    -- Can't read SSH private key
    assertPermissionDenied $
        readFile privateKey

    -- Can't write to SSH public key
    assertPermissionDenied $
        withFile publicKey WriteMode $ \fd ->
            hPutStrLn fd "Oops, key gone!"

    -- Can't remove SSH public key
    assertPermissionDenied $
        removeFile publicKey

    -- Can't create files in homedir
    assertPermissionDenied $
        writeFile (homeDir </> "whoops.txt") "This file should not exist!"

    -- Can't read files from (real) /etc
    assertPermissionDenied $
        readFile ("/etc" </> "hosts")

  where
    lookupAccessFsFlags version = case lookupMax version accessFsFlags of
        Nothing -> fail "Unsupported ABI version"
        Just flags -> return flags

    lookupAccessNetFlags version = case lookupMax version accessNetFlags of
        Nothing -> fail "Unsupported ABI version"
        Just flags -> return flags

    assertPermissionDenied act = handleJust permissionDenied return $ do
        _ <- act
        fail "Expected permission error"
    permissionDenied e = if isPermissionError e then Just () else Nothing

    lookupMax key = fmap snd
                  . listToMaybe
                  . reverse
                  . sort
                  . filter (\(k, _) -> key >= k)
```
