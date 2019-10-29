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
    ( -- * Encoding
      encode
    , encodeWith
    , encodeFile
    , encodeFileWith
      -- * Decoding
    , decodeEither'
    , decodeFileEither
    , decodeFileWithWarnings
    , decodeThrow
    , decodeFileThrow
      -- ** More control over decoding
    , decodeHelper
      -- * Types
    , Value (..)
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
      -- ** With helpers (since 0.8.23)
    , withObject
    , withText
    , withArray
    , withScientific
    , withBool
      -- * Parsing
    , parseMonad
    , parseEither
    , parseMaybe
      -- * Classes
    , ToJSON (..)
    , FromJSON (..)
      -- * Custom encoding
    , isSpecialString
    , EncodeOptions
    , defaultEncodeOptions
    , defaultStringStyle
    , setStringStyle
    , setFormat
    , FormatOptions
    , defaultFormatOptions
    , setWidth
      -- * Deprecated
    , decode
    , decodeFile
    , decodeEither
    ) where
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative((<$>))
#endif
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadThrow, throwM)
import Data.Aeson
    ( Value (..), ToJSON (..), FromJSON (..), object
    , (.=) , (.:) , (.:?) , (.!=)
    , Object, Array
    , withObject, withText, withArray, withScientific, withBool
    )
import Data.Aeson.Types (parseMaybe, parseEither, Parser)
import Data.ByteString (ByteString)
import Data.Conduit ((.|), runConduitRes)
import qualified Data.Conduit.List as CL
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)
import Data.Text (Text)

import Data.Yaml.Internal
import Text.Libyaml hiding (encode, decode, encodeFile, decodeFile, encodeWith, encodeFileWith)
import qualified Text.Libyaml as Y

-- | Set the string style in the encoded YAML. This is a function that decides
-- for each string the type of YAML string to output.
--
-- __WARNING__: You must ensure that special strings (like @"yes"@\/@"no"@\/@"null"@\/@"1234"@) are not encoded with the 'Plain' style, because
-- then they will be decoded as boolean, null or numeric values. You can use 'isSpecialString' to detect them.
--
-- By default, strings are encoded as follows:
--
-- * Any string containing a newline character uses the 'Literal' style
--
-- * Otherwise, any special string (see 'isSpecialString') uses 'SingleQuoted'
--
-- * Otherwise, use 'Plain'
--
-- @since 0.10.2.0
setStringStyle :: (Text -> ( Tag, Style )) -> EncodeOptions -> EncodeOptions
setStringStyle s opts = opts { encodeOptionsStringStyle = s }

-- | Set the encoding formatting for the encoded YAML. By default, this is `defaultFormatOptions`.
--
-- @since 0.10.2.0
setFormat :: FormatOptions -> EncodeOptions -> EncodeOptions
setFormat f opts = opts { encodeOptionsFormat = f }

-- |
-- @since 0.10.2.0
data EncodeOptions = EncodeOptions
  { encodeOptionsStringStyle :: Text -> ( Tag, Style )
  , encodeOptionsFormat :: FormatOptions
  }

-- |
-- @since 0.10.2.0
defaultEncodeOptions :: EncodeOptions
defaultEncodeOptions = EncodeOptions
  { encodeOptionsStringStyle = defaultStringStyle
  , encodeOptionsFormat = defaultFormatOptions
  }

-- | Encode a value into its YAML representation.
encode :: ToJSON a => a -> ByteString
encode = encodeWith defaultEncodeOptions

-- | Encode a value into its YAML representation with custom styling.
--
-- @since 0.10.2.0
encodeWith :: ToJSON a => EncodeOptions -> a -> ByteString
encodeWith opts obj = unsafePerformIO $ runConduitRes
    $ CL.sourceList (objToStream (encodeOptionsStringStyle opts) $ toJSON obj)
   .| Y.encodeWith (encodeOptionsFormat opts)

-- | Encode a value into its YAML representation and save to the given file.
encodeFile :: ToJSON a => FilePath -> a -> IO ()
encodeFile = encodeFileWith defaultEncodeOptions

-- | Encode a value into its YAML representation with custom styling and save to the given file.
--
-- @since 0.10.2.0
encodeFileWith :: ToJSON a => EncodeOptions -> FilePath -> a -> IO ()
encodeFileWith opts fp obj = runConduitRes
    $ CL.sourceList (objToStream (encodeOptionsStringStyle opts) $ toJSON obj)
   .| Y.encodeFileWith (encodeOptionsFormat opts) fp

decode :: FromJSON a
       => ByteString
       -> Maybe a
decode bs = unsafePerformIO
          $ either (const Nothing) snd
          <$> decodeHelper_ (Y.decode bs)
{-# DEPRECATED decode "Please use decodeEither or decodeThrow, which provide information on how the decode failed" #-}

decodeFile :: FromJSON a
           => FilePath
           -> IO (Maybe a)
decodeFile fp = (fmap snd <$> decodeHelper (Y.decodeFile fp)) >>= either throwIO (return . either (const Nothing) id)
{-# DEPRECATED decodeFile "Please use decodeFileEither, which does not confused type-directed and runtime exceptions." #-}

-- | A version of 'decodeFile' which should not throw runtime exceptions.
--
-- @since 0.8.4
decodeFileEither
    :: FromJSON a
    => FilePath
    -> IO (Either ParseException a)
decodeFileEither = fmap (fmap snd) . decodeFileWithWarnings

-- | A version of `decodeFileEither` that returns warnings along with the parse
-- result.
--
-- @since 0.10.0
decodeFileWithWarnings
    :: FromJSON a
    => FilePath
    -> IO (Either ParseException ([Warning], a))
decodeFileWithWarnings = decodeHelper_ . Y.decodeFile

decodeEither :: FromJSON a => ByteString -> Either String a
decodeEither bs = unsafePerformIO
                $ either (Left . prettyPrintParseException) id
                <$> (fmap snd <$> decodeHelper (Y.decode bs))
{-# DEPRECATED decodeEither "Please use decodeEither' or decodeThrow, which provide more useful failures" #-}

-- | More helpful version of 'decodeEither' which returns the 'YamlException'.
--
-- @since 0.8.3
decodeEither' :: FromJSON a => ByteString -> Either ParseException a
decodeEither' = either Left (either (Left . AesonException) Right)
              . unsafePerformIO
              . fmap (fmap snd) . decodeHelper
              . Y.decode

-- | A version of 'decodeEither'' lifted to MonadThrow
--
-- @since 0.8.31
decodeThrow :: (MonadThrow m, FromJSON a) => ByteString -> m a
decodeThrow = either throwM return . decodeEither'

-- | A version of 'decodeFileEither' lifted to MonadIO
--
-- @since 0.8.31
decodeFileThrow :: (MonadIO m, FromJSON a) => FilePath -> m a
decodeFileThrow f = liftIO $ decodeFileEither f >>= either throwIO return

-- | Construct a new 'Value' from a list of 'Value's.
array :: [Value] -> Value
array = Array . V.fromList

#if MIN_VERSION_base(4, 13, 0)
parseMonad :: MonadFail m => (a -> Parser b) -> a -> m b
#else
parseMonad :: Monad m => (a -> Parser b) -> a -> m b
#endif
parseMonad p = either fail return . parseEither p
{-# DEPRECATED parseMonad "With the MonadFail split, this function is going to be removed in the future. Please migrate to parseEither." #-}
