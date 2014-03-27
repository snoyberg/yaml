{-# LANGUAGE FlexibleInstances #-}
-- | NOTE: This module is a highly experimental preview release. It may change
-- drastically, or be entirely removed, in a future release.
module Data.Yaml.Builder
    ( YamlBuilder (..)
    , ToYaml (..)
    , mapping
    , array
    , string
    , toByteString
    , writeYamlFile
    , (.=)
    ) where

import Data.Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Text.Libyaml
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Conduit.List as CL
import System.IO.Unsafe (unsafePerformIO)
import Control.Arrow (second)
import qualified Data.ByteString.Char8 as S8
import Control.Monad.Trans.Resource (runResourceT)

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
string t = YamlBuilder (EventScalar (encodeUtf8 t) StrTag PlainNoTag Nothing:)

toEvents :: YamlBuilder -> [Event]
toEvents (YamlBuilder front) =
    EventStreamStart : EventDocumentStart : front [EventDocumentEnd, EventStreamEnd]

toSource :: (Monad m, ToYaml a) => a -> Source m Event
toSource = mapM_ yield . toEvents . toYaml

toByteString :: ToYaml a => a -> ByteString
toByteString yb = unsafePerformIO $ runResourceT $ toSource yb $$ encode

writeYamlFile :: ToYaml a => FilePath -> a -> IO ()
writeYamlFile fp yb = runResourceT $ toSource yb $$ encodeFile fp
