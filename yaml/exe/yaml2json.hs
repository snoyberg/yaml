{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (putStr, getContents)

import Data.Aeson           (encode, Value)
import Data.ByteString      (getContents)
import Data.ByteString.Lazy (putStr)
import Data.List            (intercalate)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup       (Semigroup(..))
#endif
import Data.Version         (Version(versionBranch))
import Data.Yaml            (decodeFileEither, decodeEither')

import Options.Applicative
  ( Parser
  , action, execParser, header, help, helper, hidden
  , info, infoOption, long, metavar, short, strArgument
  )

import System.Exit
import System.IO (stderr, hPrint)

import qualified Paths_yaml as Paths

newtype Options = Options
  { optInput :: Maybe FilePath
      -- ^ Nothing means @stdin@.
  }

-- | Name of the executable.

self :: String
self = "yaml2json"

-- | Homepage for this program.

homepage :: String
homepage = "https://github.com/snoyberg/yaml/"

-- | Version in @x.y.z@ format.

version :: String
version = intercalate "." $ map show $ versionBranch Paths.version

-- | Parse options; handle parse errors, @--help@, @--version@.

options :: IO Options
options =
  execParser $
    info (helper <*> versionOption <*> numericVersionOption <*> programOptions)
         (header "Reads given YAML document and prints it JSON format to standard out.")

  where
  versionOption =
    infoOption (unwords versionWords)
      $  long "version"
      <> short 'V'
      <> hidden
      <> help "Show version info."
  versionWords = [ self, "version", version, homepage ]

  numericVersionOption =
    infoOption version
      $  long "numeric-version"
      <> hidden
      <> help "Show just version number."

  programOptions = Options <$> oInput

  oInput :: Parser (Maybe FilePath)
  oInput = dashToStdin <$> do
    strArgument
      $  metavar "FILE"
      <> action "file"
      <> help "The input file containing the YAML document; use '-' for stdin."
    where
    -- dash is interpreted as stdin
    dashToStdin = \case
          "-"  -> Nothing
          file -> Just file

showJSON :: Show a => Either a Value -> IO b
showJSON = \case
  Left  err -> hPrint stderr err >> exitFailure
  Right res -> putStr (encode (res :: Value)) >> exitSuccess

main :: IO ()
main = do
  Options{ optInput } <- options
  showJSON =<< case optInput of
    -- strict getContents will read in all of stdin at once
    Nothing -> decodeEither' <$> getContents
    Just f  -> decodeFileEither f
