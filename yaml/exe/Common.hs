{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | Code shared between @yaml2json@ and @json2yaml@.

module Common where

import Data.List            ( intercalate )
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup       ( Semigroup(..) )
#endif
import Data.Version         ( Version(versionBranch) )

import Options.Applicative  ( Parser, help, hidden, infoOption, long, short )

import qualified Paths_yaml as Paths

-- * Version info and option
---------------------------------------------------------------------------

-- | Homepage for @yaml2json@ and @json2yaml@.

homepage :: String
homepage = "https://github.com/snoyberg/yaml/"

-- | Version in @x.y.z@ format.

version :: String
version = intercalate "." $ map show $ versionBranch Paths.version

-- | The version header including given program name and 'homepage'.

versionText :: String -> String
versionText self = unwords
  [ self
  , "version"
  , version
  , homepage  -- concat [ "<", homepage, ">" ]
  ]

-- | Option @--numeric-version@.

numericVersionOption :: Parser (a -> a)
numericVersionOption =
  infoOption version
    $  long "numeric-version"
    <> hidden
    <> help "Show just version number."

-- | Option @--version@.

versionOption :: String -> Parser (a -> a)
versionOption self =
  infoOption (versionText self)
    $  long "version"
    <> short 'V'
    <> hidden
    <> help "Show version info."

-- * Misc
---------------------------------------------------------------------------

-- | @Just@ unless argument is @"-"@ (denoting @stdin@ or @stdout@).

dashToNothing :: FilePath -> Maybe FilePath
dashToNothing = \case
  "-"  -> Nothing
  file -> Just file
