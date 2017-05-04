{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config2 where

import           Data.Yaml (FromJSON(..), (.:))
import qualified Data.Yaml as Y
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Aeson (withObject)
import           Data.Map (Map)
import           Data.Set (Set)
import           GHC.Generics (Generic)
import           Control.Applicative ((<$>), (<*>))
import           Prelude -- Ensure Applicative is in scope and we have no warnings, before/after AMP.

data Flag = Awesome | Okay | Hack1
  deriving (Eq, Show, Ord, Generic)

instance FromJSON Flag

data Config =
  Config {
    txtKey :: Text
  , intKey :: Int
  , dblKey :: Double
  , txtListKey :: [Text]
  , mapIntsKey :: Map Text Int
  , setEnumsKey :: Set Flag
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON =
    withObject "Config" (\o -> 
      Config
        <$> o .: "txtKey"
        <*> o .: "intKey"
        <*> o .: "dblKey"
        <*> o .: "txtListKey"
        <*> o .: "mapIntsKey"
        <*> o .: "setEnumsKey")

main :: IO ()
main = do
  let jsonBS :: ByteString
      jsonBS = "txtKey: txtval\n\
               \intKey: 1\n\
               \dblKey: 2.0\n\
               \txtListKey:\n\
               \  - txtval1\n\
               \  - txtval2\n\
               \mapIntsKey:\n\
               \  k1: 1\n\
               \  k2: 2\n\
               \setEnumsKey: [Awesome, Hack1]"
  print (Y.decodeEither jsonBS :: Either String Config)
