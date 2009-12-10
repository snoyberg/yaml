{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Object.Yaml
    ( -- * The event stream
      Event (..)
    , Style (..)
      -- * 'YamlObject' definition
    , Yaml (..)
    , YamlObject
      -- * Performing conversions
    , eventsToYamlObject
      -- * Exceptions
    , YamlException (..)
    ) where

import Data.ByteString (ByteString)
import Data.Object
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
    | EventScalar ByteString ByteString Style
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

data Yaml = Yaml
    { value :: ByteString
    , tag :: String
    , style :: Style
    }
    deriving (Show, Eq)
instance ConvertSuccess Yaml Event where
    convertSuccess (Yaml v t s) = EventScalar v (convertSuccess t) s

instance ConvertSuccess Yaml [Char] where
    convertSuccess = convertSuccess . value
instance ConvertSuccess [Char] Yaml where
    convertSuccess t = Yaml (convertSuccess t) "" Any

instance ConvertSuccess Yaml Text where
    convertSuccess = convertSuccess . value
instance ConvertSuccess Text Yaml where
    convertSuccess t = Yaml (convertSuccess t) "" Any

-- FIXME the following are incredibly stupid conversions which ignore tag
-- and style information.
instance ConvertSuccess Yaml Scalar where
    convertSuccess = Text . convertSuccess
instance ConvertSuccess Scalar Yaml where
    convertSuccess s = convertSuccess (convertSuccess s :: Text)

type YamlObject = Object Yaml Yaml

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
instance ConvertSuccess (Object Yaml Yaml) [Event] where
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
        helperPairs :: (Yaml, YamlObject) -> [Event] -> [Event]
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
            return (Scalar $ Yaml v (convertSuccess t) s, rest)
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
                => ([(Yaml, YamlObject)] -> [(Yaml, YamlObject)])
                -> [Event]
                -> m ([(Yaml, YamlObject)], [Event])
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

instance ConvertSuccess (Object Yaml Yaml) (Object Text Text) where
    convertSuccess = mapKeysValues convertSuccess convertSuccess
instance ConvertSuccess (Object Text Text) (Object Yaml Yaml) where
    convertSuccess = mapKeysValues convertSuccess convertSuccess

instance ConvertSuccess (Object Yaml Yaml) (Object String Scalar) where
    convertSuccess = mapKeysValues convertSuccess convertSuccess
instance ConvertSuccess (Object String Scalar) (Object Yaml Yaml) where
    convertSuccess = mapKeysValues convertSuccess convertSuccess
