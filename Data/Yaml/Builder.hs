{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | NOTE: This module is a highly experimental preview release. It may change
-- drastically, or be entirely removed, in a future release.
module Data.Yaml.Builder
    ( YamlBuilder (..)
    , ToYaml (..)
    , mapping
    , array
    , string
    , bool
    , null
    , scientific
    , number
    , toByteString
    , writeYamlFile
    , (.=)
    ) where

import Data.Conduit
import Data.ByteString (ByteString)
import Text.Libyaml
import Data.Yaml.Internal
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Aeson.Types (Value(..))
import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.IO.Unsafe (unsafePerformIO)
import Control.Arrow (second)
import qualified Data.ByteString.Char8 as S8
import Control.Monad.Trans.Resource (runResourceT)
#if MIN_VERSION_aeson(0, 7, 0)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)
import Data.Aeson.Encode (encodeToTextBuilder)
#else
import qualified Data.ByteString.Char8 as S8
#endif
import Prelude hiding (null)

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

mapping :: [(Text, YamlBuilder)] -> YamlBuilder
mapping pairs = YamlBuilder $ \rest ->
    EventMappingStart Nothing : foldr addPair (EventMappingEnd : rest) pairs
  where
    addPair (key, YamlBuilder value) after
        = EventScalar (encodeUtf8 key) StrTag PlainNoTag Nothing
        : value after

array :: [YamlBuilder] -> YamlBuilder
array bs =
    YamlBuilder $ (EventSequenceStart Nothing:) . flip (foldr go) bs . (EventSequenceEnd:)
  where
    go (YamlBuilder b) rest = b rest

string :: Text -> YamlBuilder
-- Empty strings need special handling to ensure they get quoted. This avoids:
-- https://github.com/snoyberg/yaml/issues/24
string ""  = YamlBuilder (EventScalar "" NoTag SingleQuoted Nothing :)
string s   =
    YamlBuilder (event :)
  where
    event
        -- Make sure that special strings are encoded as strings properly.
        -- See: https://github.com/snoyberg/yaml/issues/31
        | s `HashSet.member` specialStrings || isNumeric s = EventScalar (encodeUtf8 s) NoTag SingleQuoted Nothing
        | otherwise = EventScalar (encodeUtf8 s) StrTag PlainNoTag Nothing
 
-- Use aeson's implementation which gets rid of annoying decimal points
scientific :: Scientific -> YamlBuilder
scientific n = YamlBuilder (EventScalar (TE.encodeUtf8 $ TL.toStrict $ toLazyText $ encodeToTextBuilder (Number n)) IntTag PlainNoTag Nothing :)

{-# DEPRECATED number "Use scientific" #-}
#if MIN_VERSION_aeson(0,7,0)
number :: Scientific -> YamlBuilder
number = scientific
#else
number :: Number -> YamlBuilder
number n rest = YamlBuilder (EventScalar (S8.pack $ show n) IntTag PlainNoTag Nothing :)
#endif

bool :: Bool -> YamlBuilder
bool True   = YamlBuilder (EventScalar "true" BoolTag PlainNoTag Nothing :)
bool False  = YamlBuilder (EventScalar "false" BoolTag PlainNoTag Nothing :)

null :: YamlBuilder
null = YamlBuilder (EventScalar "null" NullTag PlainNoTag Nothing :)

toEvents :: YamlBuilder -> [Event]
toEvents (YamlBuilder front) =
    EventStreamStart : EventDocumentStart : front [EventDocumentEnd, EventStreamEnd]

toSource :: (Monad m, ToYaml a) => a -> Source m Event
toSource = mapM_ yield . toEvents . toYaml

toByteString :: ToYaml a => a -> ByteString
toByteString yb = unsafePerformIO $ runResourceT $ toSource yb $$ encode

writeYamlFile :: ToYaml a => FilePath -> a -> IO ()
writeYamlFile fp yb = runResourceT $ toSource yb $$ encodeFile fp
