{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Aeson           (encode, Value)
import qualified Data.ByteString      as S (getContents)
import qualified Data.ByteString.Lazy as L (putStr, writeFile)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup       (Semigroup(..))
#endif
import Data.Yaml            (decodeFileEither, decodeEither')
import System.Exit          (die)

import Options.Applicative
  ( Parser
  , action, execParser, headerDoc, help, helper, info
  , long, metavar, short, strArgument, strOption, value
  )
import Options.Applicative.Help.Pretty
  ( vcat, text )

import Common
  ( versionText, versionOption, numericVersionOption, dashToNothing )

data Options = Options
  { optInput :: Maybe FilePath
      -- ^ 'Nothing' means @stdin@.
  , optOutput :: Maybe FilePath
      -- ^ 'Nothing' means @stdout@.
  }

-- | Name of the executable.

self :: String
self = "yaml2json"

-- | Parse options; handle parse errors, @--help@, @--version@.

options :: IO Options
options =
  execParser $
    info (helper <*> versionOption self <*> numericVersionOption <*> programOptions)
         (headerDoc hdoc)

  where
  hdoc = Just $ vcat $ map text
    [ versionText self
    , "Convert YAML to JSON."
    ]

  programOptions = Options <$> oInput <*> oOutput

  oInput :: Parser (Maybe FilePath)
  oInput = dashToNothing <$> do
    strArgument
      $  metavar "IN"
      <> value "-"
      <> action "file"
      <> help "The input file containing the YAML document; use '-' for stdin (default)."

  oOutput :: Parser (Maybe FilePath)
  oOutput = dashToNothing <$> do
    strOption
      $  long "output"
      <> short 'o'
      <> value "-"
      <> metavar "OUT"
      <> action "file"
      <> help "The file to hold the produced JSON document; use '-' for stdout (default)."


main :: IO ()
main = do
  Options{ optInput, optOutput } <- options

  -- Input YAML.
  result <- case optInput of
    -- strict getContents will read in all of stdin at once
    Nothing -> decodeEither' <$> S.getContents
    Just f  -> decodeFileEither f

  -- Output JSON.
  case result of
    Left  err -> die $ show err
    Right val -> do
      let json = encode (val :: Value)
      maybe L.putStr L.writeFile optOutput json
