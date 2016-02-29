{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides a high-level interface for processing YAML files.
--
-- This module reuses most of the infrastructure from the @aeson@ package.
-- This means that you can use all of the existing tools for JSON
-- processing for processing YAML files. As a result, much of the
-- documentation below mentions JSON; do not let that confuse you, it's
-- intentional.
--
-- For the most part, YAML content translates directly into JSON, and
-- therefore there is very little data loss. If you need to deal with YAML
-- more directly (e.g., directly deal with aliases), you should use the
-- "Text.Libyaml" module instead.
--
-- For documentation on the @aeson@ types, functions, classes, and
-- operators, please see the @Data.Aeson@ module of the @aeson@ package.
--
-- Look in the examples directory of the source repository for some initial
-- pointers on how to use this library.

#if (defined (ghcjs_HOST_OS))
module Data.Yaml {-# WARNING "GHCJS is not supported yet (will break at runtime once called)." #-}
#else
module Data.Yaml
#endif
    ( -- * Types
      Value (..)
    , Parser
    , Object
    , Array
    , ParseException(..)
    , prettyPrintParseException
    , YamlException (..)
    , YamlMark (..)
      -- * Constructors and accessors
    , object
    , array
    , (.=)
    , (.:)
    , (.:?)
    , (.!=)
      -- * Parsing
    , parseMonad
    , parseEither
    , parseMaybe
      -- * Classes
    , ToJSON (..)
    , FromJSON (..)
      -- * Encoding/decoding
    , encode
    , encodeFile
    , decode
    , decodeFile
      -- ** Better error information
    , decodeEither
    , decodeEither'
    , decodeFileEither
      -- ** More control over decoding
    , decodeHelper
    ) where
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative((<$>))
#endif
import Control.Exception
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
    ( Value (..), ToJSON (..), FromJSON (..), object
    , (.=) , (.:) , (.:?) , (.!=)
    , Object, Array
    )
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Aeson.Types (Pair, parseMaybe, parseEither, Parser)
import Data.ByteString (ByteString)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HashSet
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)

import Data.Yaml.Internal
import Text.Libyaml hiding (encode, decode, encodeFile, decodeFile)
import qualified Text.Libyaml as Y

encode :: ToJSON a => a -> ByteString
encode obj = unsafePerformIO $
    runResourceT $ CL.sourceList (objToEvents $ toJSON obj)
                C.$$ Y.encode

encodeFile :: ToJSON a => FilePath -> a -> IO ()
encodeFile fp obj = runResourceT
            $ CL.sourceList (objToEvents $ toJSON obj)
         C.$$ Y.encodeFile fp

objToEvents :: Value -> [Y.Event]
objToEvents o = (:) EventStreamStart
              . (:) EventDocumentStart
              $ objToEvents' o
              [ EventDocumentEnd
              , EventStreamEnd
              ]

{- FIXME
scalarToEvent :: YamlScalar -> Event
scalarToEvent (YamlScalar v t s) = EventScalar v t s Nothing
-}

objToEvents' :: Value -> [Y.Event] -> [Y.Event]
--objToEvents' (Scalar s) rest = scalarToEvent s : rest
objToEvents' (Array list) rest =
    EventSequenceStart Nothing
  : foldr objToEvents' (EventSequenceEnd : rest) (V.toList list)
objToEvents' (Object pairs) rest =
    EventMappingStart Nothing
  : foldr pairToEvents (EventMappingEnd : rest) (M.toList pairs)

-- Empty strings need special handling to ensure they get quoted. This avoids:
-- https://github.com/snoyberg/yaml/issues/24
objToEvents' (String "") rest = EventScalar "" NoTag SingleQuoted Nothing : rest

objToEvents' (String s) rest =
    event : rest
  where
    event
        -- Make sure that special strings are encoded as strings properly.
        -- See: https://github.com/snoyberg/yaml/issues/31
        | s `HashSet.member` specialStrings || isNumeric s = EventScalar (encodeUtf8 s) NoTag SingleQuoted Nothing
        | otherwise = EventScalar (encodeUtf8 s) StrTag PlainNoTag Nothing
objToEvents' Null rest = EventScalar "null" NullTag PlainNoTag Nothing : rest
objToEvents' (Bool True) rest = EventScalar "true" BoolTag PlainNoTag Nothing : rest
objToEvents' (Bool False) rest = EventScalar "false" BoolTag PlainNoTag Nothing : rest
-- Use aeson's implementation which gets rid of annoying decimal points
objToEvents' n@Number{} rest = EventScalar (TE.encodeUtf8 $ TL.toStrict $ toLazyText $ encodeToTextBuilder n) IntTag PlainNoTag Nothing : rest

pairToEvents :: Pair -> [Y.Event] -> [Y.Event]
pairToEvents (k, v) rest =
    EventScalar (encodeUtf8 k) StrTag PlainNoTag Nothing
  : objToEvents' v rest

decode :: FromJSON a
       => ByteString
       -> Maybe a
decode bs = unsafePerformIO
          $ either (const Nothing) id
          <$> decodeHelper_ (Y.decode bs)

decodeFile :: FromJSON a
           => FilePath
           -> IO (Maybe a)
decodeFile fp = decodeHelper (Y.decodeFile fp) >>= either throwIO (return . either (const Nothing) id)

-- | A version of 'decodeFile' which should not throw runtime exceptions.
--
-- Since 0.8.4
decodeFileEither
    :: FromJSON a
    => FilePath
    -> IO (Either ParseException a)
decodeFileEither = decodeHelper_ . Y.decodeFile

decodeEither :: FromJSON a => ByteString -> Either String a
decodeEither bs = unsafePerformIO
                $ either (Left . prettyPrintParseException) id
                <$> decodeHelper (Y.decode bs)

-- | More helpful version of 'decodeEither' which returns the 'YamlException'.
--
-- Since 0.8.3
decodeEither' :: FromJSON a => ByteString -> Either ParseException a
decodeEither' = either Left (either (Left . AesonException) Right)
              . unsafePerformIO
              . decodeHelper
              . Y.decode

array :: [Value] -> Value
array = Array . V.fromList

parseMonad :: Monad m => (a -> Parser b) -> a -> m b
parseMonad p = either fail return . parseEither p
