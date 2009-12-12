{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Object.Yaml
    ( -- * The event stream
      Event (..)
    , Style (..)
    , Tag (..)
      -- * 'YamlObject' definition
    , YamlDoc (..)
    , YamlScalar (..)
    , YamlObject
      -- * Performing conversions
    , eventsToYamlObject
      -- * Exceptions
    , YamlException (..)
      -- * Re-export
    , module Data.Object.Base
    ) where

import Data.ByteString (ByteString)
import Data.Object.Base
import Data.Object.Text
import Data.Object.Scalar
import Data.Convertible.Text
import Data.Attempt

import Data.Typeable (Typeable)
import Control.Exception (Exception)

data Event =
    EventNone
    | EventStreamStart
    | EventStreamEnd
    | EventDocumentStart
    | EventDocumentEnd
    | EventAlias
    | EventScalar ByteString Tag Style
    | EventSequenceStart
    | EventSequenceEnd
    | EventMappingStart
    | EventMappingEnd
    deriving (Show, Eq)

data Style = Any
           | Plain
           | SingleQuoted
           | DoubleQuoted
           | Literal
           | Folded
    deriving (Show, Eq, Enum, Bounded, Ord)

data Tag = StrTag
         | FloatTag
         | NullTag
         | BoolTag
         | SetTag
         | IntTag
         | SeqTag
         | MapTag
         | UriTag String
         | NoTag
    deriving Eq
instance Show Tag where
    show StrTag = "tag:yaml.org,2002:str"
    show FloatTag = "tag:yaml.org,2002:float"
    show NullTag = "tag:yaml.org,2002:null"
    show BoolTag = "tag:yaml.org,2002:bool"
    show SetTag = "tag:yaml.org,2002:set"
    show IntTag = "tag:yaml.org,2002:int"
    show SeqTag = "tag:yaml.org,2002:seq"
    show MapTag = "tag:yaml.org,2002:map"
    show (UriTag s) = s
    show NoTag = ""
instance ConvertSuccess Tag [Char] where
    convertSuccess = show
instance ConvertSuccess ByteString Tag where
    convertSuccess = (convertSuccess :: String -> Tag)
                   . convertSuccess
instance ConvertSuccess [Char] Tag where
    convertSuccess "tag:yaml.org,2002:str" = StrTag
    convertSuccess "tag:yaml.org,2002:float" = FloatTag
    convertSuccess "tag:yaml.org,2002:null" = NullTag
    convertSuccess "tag:yaml.org,2002:bool" = BoolTag
    convertSuccess "tag:yaml.org,2002:set" = SetTag
    convertSuccess "tag:yaml.org,2002:int" = IntTag
    convertSuccess "tag:yaml.org,2002:seq" = SeqTag
    convertSuccess "tag:yaml.org,2002:map" = MapTag
    convertSuccess "" = NoTag
    convertSuccess s = UriTag s

newtype YamlDoc = YamlDoc { unYamlDoc :: ByteString }

data YamlScalar = YamlScalar
    { value :: ByteString
    , tag :: Tag
    , style :: Style
    }
    deriving (Show, Eq)
instance ConvertSuccess YamlScalar Event where
    convertSuccess (YamlScalar v t s) = EventScalar v (convertSuccess t) s

instance ConvertSuccess YamlScalar [Char] where
    convertSuccess = convertSuccess . value
instance ConvertSuccess [Char] YamlScalar where
    convertSuccess t = YamlScalar (convertSuccess t) NoTag Any

instance ConvertSuccess YamlScalar Text where
    convertSuccess = convertSuccess . value
instance ConvertSuccess Text YamlScalar where
    convertSuccess t = YamlScalar (convertSuccess t) NoTag Any

-- FIXME the following are incredibly stupid conversions which ignore tag
-- and style information.
instance ConvertSuccess YamlScalar Scalar where
    convertSuccess = Text . convertSuccess
instance ConvertSuccess Scalar YamlScalar where
    convertSuccess s = convertSuccess (convertSuccess s :: Text)

type YamlObject = Object YamlScalar YamlScalar

data YamlException =
    YamlParserException
        { parserProblem :: String
        , parserContext :: String
        , parserOffset :: Int
        }
    | YamlEmitterException { emitterProblem :: String }
    | YamlOutOfMemory
    | YamlInvalidEventStreamBeginning [Event]
    | YamlInvalidEventStreamEnd [Event]
    | YamlPrematureEventStreamEnd
    | YamlNonScalarKey
    | YamlInvalidStartingEvent Event
    deriving (Show, Typeable)
instance Exception YamlException

-- Emit a YamlObject to an event stream
instance ConvertSuccess YamlObject [Event] where
    convertSuccess o = EventStreamStart
                     : EventDocumentStart
                     : helper o [EventDocumentEnd, EventStreamEnd] where
        helper :: YamlObject -> [Event] -> [Event]
        helper (Scalar y) rest = convertSuccess y : rest
        helper (Sequence ys) rest =
            EventSequenceStart
            : foldr ($) (EventSequenceEnd : rest) (map helper ys)
        helper (Mapping pairs) rest =
            EventMappingStart
            : foldr ($) (EventMappingEnd : rest) (map helperPairs pairs)
        helperPairs :: (YamlScalar, YamlObject) -> [Event] -> [Event]
        helperPairs (k, v) rest = convertSuccess k : helper v rest

-- Parse a YamlObject from an event stream
eventsToYamlObject :: MonadFailure YamlException m
                   => [Event]
                   -> m YamlObject
eventsToYamlObject (EventStreamStart:EventDocumentStart:events) = h1 events
      where
        h1 :: MonadFailure YamlException m => [Event] -> m YamlObject
        h1 es = do
            (yo, es') <- h2 es
            case es' of
                [EventDocumentEnd, EventStreamEnd] -> return yo
                _ -> failure $ YamlInvalidEventStreamEnd es'
        h2 :: MonadFailure YamlException m
           => [Event]
           -> m (YamlObject, [Event])
        h2 [] = failure YamlPrematureEventStreamEnd
        h2 (EventScalar v t s:rest) =
            return (Scalar $ YamlScalar v (convertSuccess t) s, rest)
        h2 (EventSequenceStart:es) = do
            (yos, rest) <- readSeq id es
            return (Sequence yos, rest)
        h2 (EventMappingStart:es) = do
            (ypairs, rest) <- readMap id es
            return (Mapping ypairs, rest)
        h2 (e:_) = failure $ YamlInvalidStartingEvent e

        readSeq :: MonadFailure YamlException m
                => ([YamlObject] -> [YamlObject])
                -> [Event]
                -> m ([YamlObject], [Event])
        readSeq _ [] = failure YamlPrematureEventStreamEnd
        readSeq f (EventSequenceEnd:rest) = return (f [], rest)
        readSeq f es = do
            (next, rest) <- h2 es
            readSeq (f . ((:) next)) rest

        readMap :: MonadFailure YamlException m
                => ([(YamlScalar, YamlObject)] -> [(YamlScalar, YamlObject)])
                -> [Event]
                -> m ([(YamlScalar, YamlObject)], [Event])
        readMap _ [] = failure YamlPrematureEventStreamEnd
        readMap f (EventMappingEnd:rest) = return (f [], rest)
        readMap f es = do
            (key, rest) <- h2 es
            key' <- case key of
                        Scalar y -> return y
                        _ -> failure YamlNonScalarKey
            (val, rest') <- h2 rest
            readMap (f . ((:) (key', val))) rest'
eventsToYamlObject e = failure $ YamlInvalidEventStreamBeginning e

instance ConvertSuccess (Object YamlScalar YamlScalar) (Object Text Text) where
    convertSuccess = mapKeysValues convertSuccess convertSuccess
instance ConvertSuccess (Object Text Text) (Object YamlScalar YamlScalar) where
    convertSuccess = mapKeysValues convertSuccess convertSuccess

instance ConvertSuccess (Object YamlScalar YamlScalar) (Object String Scalar) where
    convertSuccess = mapKeysValues convertSuccess convertSuccess
instance ConvertSuccess (Object String Scalar) (Object YamlScalar YamlScalar) where
    convertSuccess = mapKeysValues convertSuccess convertSuccess
