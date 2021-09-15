{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (putStr, getContents)

import Data.Aeson           (encode, Value)
import Data.ByteString      (getContents)
import Data.ByteString.Lazy (putStr)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup       (Semigroup(..))
#endif
import Data.Yaml            (decodeFileEither, decodeEither')

import Options.Applicative
  ( Parser
  , action, execParser, header, help, helper, info
  , metavar, strArgument
  )

import System.Exit
import System.IO (stderr, hPrint)

import Common
  ( versionOption, numericVersionOption, dashToNothing )

newtype Options = Options
  { optInput :: Maybe FilePath
      -- ^ 'Nothing' means @stdin@.
  }

-- | Name of the executable.

self :: String
self = "yaml2json"

-- | Parse options; handle parse errors, @--help@, @--version@.

options :: IO Options
options =
  execParser $
    info (helper <*> versionOption self <*> numericVersionOption <*> programOptions)
         (header "Reads given YAML document and prints it in JSON format to standard out.")

  where
  programOptions = Options <$> oInput

  oInput :: Parser (Maybe FilePath)
  oInput = dashToNothing <$> do
    strArgument
      $  metavar "FILE"
      <> action "file"
      <> help "The input file containing the YAML document; use '-' for stdin."

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
