{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.Aeson           as J
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Yaml            as Y
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ( Semigroup(..) )
#endif

import Options.Applicative
  ( Parser, (<|>)
  , action, execParser, footerDoc, headerDoc, help, helper, hidden, info
  , long, metavar, short, strArgument, strOption, value
  )
import Options.Applicative.Help.Pretty
  ( vcat, text )

import System.Exit    ( die )

import Common         ( dashToNothing, versionText, versionOption, numericVersionOption )

data Options = Options
  { optInput  :: Maybe FilePath
      -- ^ 'Nothing' means @stdin@.
  , optOutput :: Maybe FilePath
      -- ^ 'Nothing' means @stdout@.
  }

-- | Name of the executable.

self :: String
self = "json2yaml"

-- | Parse options; handle parse errors, @--help@, @--version@.

options :: IO Options
options =
  execParser $
    info (helper <*> versionOption self <*> numericVersionOption <*> programOptions)
      $  headerDoc hdoc
      <> footerDoc fdoc

  where
  hdoc = Just $ vcat $ map text
    [ versionText self
    , "Convert JSON to YAML."
    ]
  fdoc = Just $ text $ concat
    [ "The old call pattern '"
    , self
    , " IN OUT' is also accepted, but deprecated."
    ]

  programOptions :: Parser Options
  programOptions = Options <$> oInput <*> oOutput

  oInput :: Parser (Maybe FilePath)
  oInput = dashToNothing <$> do
    strArgument
      $  metavar "IN"
      <> value "-"
      <> action "file"
      <> help "The input file containing the JSON document; use '-' for stdin (default)."

  oOutput :: Parser (Maybe FilePath)
  oOutput = dashToNothing <$> do
    oOutputExplicit <|> oOutputImplicit

  oOutputExplicit :: Parser FilePath
  oOutputExplicit =
    strOption
      $  long "output"
      <> short 'o'
      <> metavar "OUT"
      <> action "file"
      -- <> help "The file to hold the produced YAML document; use '-' for stdout (default)."

  oOutputImplicit :: Parser FilePath
  oOutputImplicit =
    strArgument
      $  metavar "OUT"
      <> value "-"
      <> action "file"
      <> hidden
      <> help "The file to hold the produced YAML document; use '-' for stdout (default)."

-- | Exit with 'self'-stamped error message.

abort :: String -> IO a
abort msg = die $ concat [ self, ": ", msg ]

main :: IO ()
main = do
  Options{ optInput, optOutput } <- options

  -- Read JSON.
  mval <- J.decode <$> maybe L.getContents L.readFile optInput

  -- Write YAML.
  case mval of
    Nothing  -> abort "Invalid input JSON"
    Just val -> case optOutput of
      Nothing -> S.putStr $ Y.encode (val :: Y.Value)
      Just f  -> Y.encodeFile f val
