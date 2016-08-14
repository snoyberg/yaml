{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Config where

import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Text.RawString.QQ
import Data.ByteString (ByteString)
import Control.Applicative
import Prelude -- Ensure Applicative is in scope and we have no warnings, before/after AMP.

configYaml :: ByteString
configYaml = [r|
resolver: lts-3.7
packages:
  - ./yesod-core
  - ./yesod-static
  - ./yesod-persistent
  - ./yesod-newsfeed
  - ./yesod-form
  - ./yesod-auth
  - ./yesod-auth-oauth
  - ./yesod-sitemap
  - ./yesod-test
  - ./yesod-bin
  - ./yesod
  - ./yesod-eventsource
  - ./yesod-websockets

# Needed for LTS 2
extra-deps:
- wai-app-static-3.1.4.1
|]

data Config =
  Config {
    resolver  :: Text
  , packages  :: [FilePath]
  , extraDeps :: [Text]
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .:   "resolver"       <*>
    v .:   "packages" <*>
    v .:   "extra-deps"
  parseJSON _ = fail "Expected Object for Config value"

main :: IO ()
main =
  print $ (Y.decode configYaml :: Maybe Config)
