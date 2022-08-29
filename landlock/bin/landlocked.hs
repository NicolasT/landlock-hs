module Main (main) where

import Control.Applicative ((<**>), many)
import Control.Exception.Base (displayException, handleJust)
import Control.Monad (forM_, unless)
import Data.List (intercalate, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down(Down))
import Data.Version (versionBranch)
import Options.Applicative (
      ParserInfo
    , ReadM
    , action
    , execParser
    , footer
    , fullDesc
    , header
    , help
    , helper
    , hidden
    , info
    , infoOption
    , long
    , metavar
    , noIntersperse
    , option
    , progDesc
    , str
    , strArgument
    )
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Posix.Process (executeFile)

import System.Landlock (
      AccessFsFlag
    , RulesetAttr(..)
    , Version
    , abiVersion
    , accessFsFlags
    , accessFsFlagIsReadOnly
    , defaultOpenPathFlags
    , getVersion
    , isSupported
    , landlock
    , pathBeneath
    , withOpenPath
    )

import qualified Paths_landlock as Paths

main :: IO ()
main = do
    hasLandlock <- isSupported
    unless hasLandlock $ do
        hPutStrLn stderr "Landlock not supported on this system"
        exitWith (ExitFailure 2)

    version <- abiVersion

    (usedVersion, allFlags) <- case lookupAccessFsFlags version of
        Nothing -> fail $ "Unable to retrieve file-system access flags for Landlock ABI " ++ show (getVersion version)
        Just r -> return r
    let roFlags = filter accessFsFlagIsReadOnly allFlags

    args <- execParser (parser version usedVersion)

    landlock (RulesetAttr allFlags) [] [] $ \addRule -> do
        forM_ (argsROPaths args) $ \path ->
            withOpenPath path defaultOpenPathFlags $ \fd ->
                addRule (pathBeneath fd roFlags) []

        forM_ (argsRWPaths args) $ \path ->
            withOpenPath path defaultOpenPathFlags $ \fd ->
                addRule (pathBeneath fd allFlags) []

    handleJust permissionDenied handlePermissionDenied $
        handleJust notFound handleNotFound $ do
            _ <- executeFile (argsCommand args) True (argsArguments args) Nothing
            fail "executeFile returned"
  where
    notFound e = if isDoesNotExistError e then Just e else Nothing
    handleNotFound e = do
        hPutStrLn stderr $ unlines [
              "Failed to execute command: " ++ displayException e
            ]
        exitWith (ExitFailure 127)
    permissionDenied e = if isPermissionError e then Just e else Nothing
    handlePermissionDenied e = do
        hPutStrLn stderr $ unlines [
              "Failed to execute command: " ++ displayException e
            , "Hint: access to the binary, the interpreter or shared libraries may be denied."
            ]
        exitWith (ExitFailure 126)

data Args = Args { argsROPaths :: [FilePath]
                 , argsRWPaths :: [FilePath]
                 , argsCommand :: FilePath
                 , argsArguments :: [String]
                 }
  deriving (Show, Eq)

parser :: Version -> Version -> ParserInfo Args
parser version usedVersion =
    info (argsParser <**> versionFlag <**> helper)
        (  fullDesc
        <> noIntersperse
        <> progDesc "Execute a command in a sandboxed environment"
        <> header (unwords [
              "Use landlocked to run a program in a sandboxed environment,"
            , "restricting access to system resources using the Linux"
            , "Landlock API."
            ])
        <> footer (unwords [
              "The command binary, its interpreter and any shared"
            , "libraries must be accessible in the sandbox."
            ])
        )
  where
    argsParser = Args <$> many (option filePath (  long "ro"
                                                <> metavar "PATH"
                                                <> help "Allow read-only access to given file or directory"
                                                <> action "default"
                                                ))
                      <*> many (option filePath (  long "rw"
                                                <> metavar "PATH"
                                                <> help "Allow write access to given file or directory"
                                                <> action "default"
                                                ))
                      <*> strArgument (  metavar "COMMAND"
                                      <> help "Command to spawn"
                                      <> action "command"
                                      )
                      <*> many (strArgument (  metavar "ARG"
                                            <> help "Arguments to pass to spawned command"
                                            ))
    versionFlag = infoOption versionString (  long "version"
                                           <> help "Show version information"
                                           <> hidden
                                           )
    versionString = unlines [
          "landlocked " ++ intercalate "." (map show (versionBranch Paths.version))
        , "System Landlock ABI version: " ++ show (getVersion version)
        , "Using Landlock ABI version: " ++ show (getVersion usedVersion)
        ]

-- This is not really useful. It could be if 'ReaderM' were 'MonadIO'
filePath :: ReadM FilePath
filePath = str

lookupAccessFsFlags :: Version -> Maybe (Version, [AccessFsFlag])
lookupAccessFsFlags v = listToMaybe
                      $ sortOn (Down . fst)
                      $ filter (\(v', _) -> v' <= v) accessFsFlags
