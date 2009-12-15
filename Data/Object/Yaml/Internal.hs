{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Object.Yaml.Internal
    ( -- * The event stream
      Event (..)
    , Style (..)
    , Tag (..)
      -- * Exceptions
    , YamlException (..)
    ) where

import Data.ByteString (ByteString)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.Convertible.Text

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
    deriving (Show, Eq)
instance ConvertSuccess Tag String where
    convertSuccess StrTag = "tag:yaml.org,2002:str"
    convertSuccess FloatTag = "tag:yaml.org,2002:float"
    convertSuccess NullTag = "tag:yaml.org,2002:null"
    convertSuccess BoolTag = "tag:yaml.org,2002:bool"
    convertSuccess SetTag = "tag:yaml.org,2002:set"
    convertSuccess IntTag = "tag:yaml.org,2002:int"
    convertSuccess SeqTag = "tag:yaml.org,2002:seq"
    convertSuccess MapTag = "tag:yaml.org,2002:map"
    convertSuccess (UriTag s) = s
    convertSuccess NoTag = ""
instance ConvertSuccess ByteString Tag where
    convertSuccess = (cs :: String -> Tag) . cs
instance ConvertSuccess String Tag where
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
