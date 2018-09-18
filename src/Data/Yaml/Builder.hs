{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | NOTE: This module is a highly experimental preview release. It may change
-- drastically, or be entirely removed, in a future release.
module Data.Yaml.Builder
    ( YamlBuilder (..)
    , ToYaml (..)
    , mapping
    , namedMapping
    , array
    , namedArray
    , string
    , namedString
    , bool
    , namedBool
    , null
    , namedNull
    , scientific
    , namedScientific
    , alias
    , number
    , toByteString
    , toByteStringWith
    , writeYamlFile
    , writeYamlFileWith
    , (.=)
    , FormatOptions
    , setWidth
    ) where

import Prelude hiding (null)

import Control.Arrow (second)
#if MIN_VERSION_aeson(1,0,0)
import Data.Aeson.Text (encodeToTextBuilder)
#else
import Data.Aeson.Encode (encodeToTextBuilder)
#endif
import Data.Aeson.Types (Value(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.Conduit
import qualified Data.HashSet as HashSet
import Data.Scientific (Scientific)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)
import System.IO.Unsafe (unsafePerformIO)

import Data.Yaml.Internal
import Text.Libyaml

(.=) :: ToYaml a => Text -> a -> (Text, YamlBuilder)
k .= v = (k, toYaml v)

newtype YamlBuilder = YamlBuilder { unYamlBuilder :: [Event] -> [Event] }

class ToYaml a where
    toYaml :: a -> YamlBuilder
instance ToYaml YamlBuilder where
    toYaml = id
instance ToYaml a => ToYaml [(Text, a)] where
    toYaml = mapping . map (second toYaml)
instance ToYaml a => ToYaml [a] where
    toYaml = array . map toYaml
instance ToYaml Text where
    toYaml = string
instance ToYaml Int where
    toYaml i = YamlBuilder (EventScalar (S8.pack $ show i) IntTag PlainNoTag Nothing:)

namedMapping :: Maybe Text -> [(Text, YamlBuilder)] -> YamlBuilder
namedMapping anchor pairs = YamlBuilder $ \rest ->
    EventMappingStart NoTag AnyMapping (unpack <$> anchor) : foldr addPair (EventMappingEnd : rest) pairs
  where
    addPair (key, YamlBuilder value) after
        = EventScalar (encodeUtf8 key) StrTag PlainNoTag Nothing
        : value after

mapping :: [(Text, YamlBuilder)] -> YamlBuilder
mapping = namedMapping Nothing

namedArray :: Maybe Text -> [YamlBuilder] -> YamlBuilder
namedArray anchor bs =
    YamlBuilder $ (EventSequenceStart NoTag AnySequence (unpack <$> anchor):) . flip (foldr go) bs . (EventSequenceEnd:)
  where
    go (YamlBuilder b) = b

array :: [YamlBuilder] -> YamlBuilder
array = namedArray Nothing

namedString :: Maybe Text -> Text -> YamlBuilder
-- Empty strings need special handling to ensure they get quoted. This avoids:
-- https://github.com/snoyberg/yaml/issues/24
namedString anchor ""  = YamlBuilder (EventScalar "" NoTag SingleQuoted (unpack <$> anchor) :)
namedString anchor s   =
    YamlBuilder (event :)
  where
    event
        -- Make sure that special strings are encoded as strings properly.
        -- See: https://github.com/snoyberg/yaml/issues/31
        | s `HashSet.member` specialStrings || isNumeric s = EventScalar (encodeUtf8 s) NoTag SingleQuoted $ unpack <$> anchor
        | otherwise = EventScalar (encodeUtf8 s) StrTag PlainNoTag $ unpack <$> anchor

string :: Text -> YamlBuilder
string = namedString Nothing
 
-- Use aeson's implementation which gets rid of annoying decimal points
namedScientific :: Maybe Text -> Scientific -> YamlBuilder
namedScientific anchor n = YamlBuilder (EventScalar (TE.encodeUtf8 $ TL.toStrict $ toLazyText $ encodeToTextBuilder (Number n)) IntTag PlainNoTag (unpack <$> anchor) :)

scientific :: Scientific -> YamlBuilder
scientific = namedScientific Nothing

{-# DEPRECATED number "Use scientific" #-}
number :: Scientific -> YamlBuilder
number = scientific

namedBool :: Maybe Text -> Bool -> YamlBuilder
namedBool anchor True   = YamlBuilder (EventScalar "true" BoolTag PlainNoTag (unpack <$> anchor) :)
namedBool anchor False  = YamlBuilder (EventScalar "false" BoolTag PlainNoTag (unpack <$> anchor) :)

bool :: Bool -> YamlBuilder
bool = namedBool Nothing

namedNull :: Maybe Text -> YamlBuilder
namedNull anchor = YamlBuilder (EventScalar "null" NullTag PlainNoTag (unpack <$> anchor) :)

null :: YamlBuilder
null = namedNull Nothing

alias :: Text -> YamlBuilder
alias anchor = YamlBuilder (EventAlias (unpack anchor) :)

toEvents :: YamlBuilder -> [Event]
toEvents (YamlBuilder front) =
    EventStreamStart : EventDocumentStart : front [EventDocumentEnd, EventStreamEnd]

toSource :: (Monad m, ToYaml a) => a -> ConduitM i Event m ()
toSource = mapM_ yield . toEvents . toYaml

toByteString :: ToYaml a => a -> ByteString
toByteString = toByteStringWith defaultFormatOptions

-- |
-- @since 0.10.2.0
toByteStringWith :: ToYaml a => FormatOptions -> a -> ByteString
toByteStringWith opts yb = unsafePerformIO $ runConduitRes $ toSource yb .| encodeWith opts

writeYamlFile :: ToYaml a => FilePath -> a -> IO ()
writeYamlFile = writeYamlFileWith defaultFormatOptions

-- |
-- @since 0.10.2.0
writeYamlFileWith :: ToYaml a => FormatOptions -> FilePath -> a -> IO ()
writeYamlFileWith opts fp yb = runConduitRes $ toSource yb .| encodeFileWith opts fp
