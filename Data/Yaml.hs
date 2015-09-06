{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
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
module Data.Yaml
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
    , decodeAll
    , decodeFile
    , decodeAllFile
      -- ** Better error information
    , decodeEither
    , decodeAllEither
    , decodeEither'
    , decodeAllEither'
    , decodeFileEither
    , decodeAllFileEither
      -- ** More control over decoding
    , decodeHelper
    ) where

import qualified Text.Libyaml as Y
import Data.Aeson
    ( Value (..), ToJSON (..), FromJSON (..), object
    , (.=) , (.:) , (.:?) , (.!=)
    , Object, Array
    )
import Data.Aeson.Types (Pair, parseMaybe, parseEither, Parser)
import Text.Libyaml hiding (encode, decode, encodeFile, decodeFile)
import Data.ByteString (ByteString)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HashSet
#if MIN_VERSION_aeson(0, 7, 0)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)
import Data.Aeson.Encode (encodeToTextBuilder)
#else
import qualified Data.ByteString.Char8 as S8
#endif
import Control.Monad.Trans.Resource (runResourceT)

import Data.Yaml.Internal

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
  : foldr ($) (EventSequenceEnd : rest) (map objToEvents' $ V.toList list)
objToEvents' (Object pairs) rest =
    EventMappingStart Nothing
  : foldr ($) (EventMappingEnd : rest) (map pairToEvents $ M.toList pairs)

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
#if MIN_VERSION_aeson(0,7,0)
-- Use aeson's implementation which gets rid of annoying decimal points
objToEvents' n@Number{} rest = EventScalar (TE.encodeUtf8 $ TL.toStrict $ toLazyText $ encodeToTextBuilder n) IntTag PlainNoTag Nothing : rest
#else
objToEvents' (Number n) rest = EventScalar (S8.pack $ show n) IntTag PlainNoTag Nothing : rest
#endif

pairToEvents :: Pair -> [Y.Event] -> [Y.Event]
pairToEvents (k, v) rest =
    EventScalar (encodeUtf8 k) StrTag PlainNoTag Nothing
  : objToEvents' v rest

decode :: FromJSON a
       => ByteString
       -> Maybe a
decode bs = unsafePerformIO
          $ fmap (either (const Nothing) id)
          $ decodeHelper_ (Y.decode bs)

decodeAll :: FromJSON a
             => ByteString
             -> Maybe [a]
decodeAll bs
  = unsafePerformIO $ do
    res <- decodeHelperOneOrMany_ False (Y.decode bs)
    return $ case res of
              Left _ -> Nothing
              Right xs -> Just xs

decodeFile :: FromJSON a
           => FilePath
           -> IO (Maybe a)
decodeFile fp = decodeHelper (Y.decodeFile fp) >>= either throwIO (return . either (const Nothing) id)

decodeAllFile :: FromJSON a
                 => FilePath
                 -> IO (Maybe [a])
decodeAllFile fp = do
  res <- decodeHelperOneOrMany False (Y.decodeFile fp)
  case res of
   Left except -> throwIO except
   Right (Left _) -> return Nothing
   Right (Right ans) -> return $ Just ans

-- | A version of 'decodeFile' which should not throw runtime exceptions.
--
-- Since 0.8.4
decodeFileEither
    :: FromJSON a
    => FilePath
    -> IO (Either ParseException a)
decodeFileEither = decodeHelper_ . Y.decodeFile

decodeAllFileEither
    :: FromJSON a
    => FilePath
    -> IO (Either ParseException [a])
decodeAllFileEither = decodeHelperOneOrMany_ False . Y.decodeFile

decodeEither :: FromJSON a => ByteString -> Either String a
decodeEither bs = unsafePerformIO
                $ fmap (either (Left . prettyPrintParseException) id)
                $ decodeHelper (Y.decode bs)

decodeAllEither :: FromJSON a => ByteString -> Either String [a]
decodeAllEither bs
  = unsafePerformIO $ do
    res <- decodeHelperOneOrMany_ False (Y.decode bs)
    return $ case res of
              Left except -> Left $ prettyPrintParseException except
              Right xs -> Right xs

-- | More helpful version of 'decodeEither' which returns the 'YamlException'.
--
-- Since 0.8.3
decodeEither' :: FromJSON a => ByteString -> Either ParseException a
decodeEither' = either Left (either (Left . AesonException) Right)
              . unsafePerformIO
              . decodeHelper
              . Y.decode

decodeAllEither' :: FromJSON a => ByteString -> Either ParseException [a]
decodeAllEither' = either Left (either (Left . AesonException) Right)
                   . unsafePerformIO
                   . decodeHelperOneOrMany False
                   . Y.decode

array :: [Value] -> Value
array = Array . V.fromList

parseMonad :: Monad m => (a -> Parser b) -> a -> m b
parseMonad p = either fail return . parseEither p
