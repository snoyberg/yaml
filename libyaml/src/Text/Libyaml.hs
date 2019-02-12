{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

-- | Low-level, streaming YAML interface. For a higher-level interface, see
-- "Data.Yaml".
module Text.Libyaml
    ( -- * The event stream
      MarkedEvent(..)
    , Event (..)
    , Style (..)
    , SequenceStyle (..)
    , MappingStyle (..)
    , Tag (..)
    , AnchorName
    , Anchor
      -- * Encoding and decoding
    , encode
    , encodeWith
    , decode
    , decodeMarked
    , encodeFile
    , decodeFile
    , decodeFileMarked
    , encodeFileWith
    , FormatOptions
    , defaultFormatOptions
    , setWidth
    , setTagRendering
    , renderScalarTags
    , renderAllTags
    , renderNoTags
    , renderUriTags
      -- * Error handling
    , YamlException (..)
    , YamlMark (..)
    ) where

import Prelude hiding (pi)

import Data.Bits ((.|.))
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
#if MIN_VERSION_base(4,7,0)
import Foreign.ForeignPtr.Unsafe
#endif
import Foreign.Marshal.Alloc
import qualified System.Posix.Internals as Posix

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Exception (mask_, throwIO, Exception, finally)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit hiding (Source, Sink, Conduit)
import Data.Data

import Data.ByteString (ByteString, packCString, packCStringLen)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as BU

data Event =
      EventStreamStart
    | EventStreamEnd
    | EventDocumentStart
    | EventDocumentEnd
    | EventAlias !AnchorName
    | EventScalar !ByteString !Tag !Style !Anchor
    | EventSequenceStart !Tag !SequenceStyle !Anchor
    | EventSequenceEnd
    | EventMappingStart !Tag !MappingStyle !Anchor
    | EventMappingEnd
    deriving (Show, Eq)

-- | Event with start and end marks.
--
-- @since 0.10.4.0
data MarkedEvent = MarkedEvent
    { yamlEvent     :: Event
    , yamlStartMark :: YamlMark
    , yamlEndMark   :: YamlMark
    }

-- | Style for scalars - e.g. quoted / folded
-- 
data Style = Any
           | Plain
           | SingleQuoted
           | DoubleQuoted
           | Literal
           | Folded
           | PlainNoTag
    deriving (Show, Read, Eq, Enum, Bounded, Ord, Data, Typeable)

-- | Style for sequences - e.g. block or flow
-- 
-- @since 0.9.0
data SequenceStyle = AnySequence | BlockSequence | FlowSequence
    deriving (Show, Eq, Enum, Bounded, Ord, Data, Typeable)

-- | Style for mappings - e.g. block or flow
-- 
-- @since 0.9.0
data MappingStyle = AnyMapping | BlockMapping | FlowMapping
    deriving (Show, Eq, Enum, Bounded, Ord, Data, Typeable)

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
    deriving (Show, Eq, Read, Data, Typeable)

tagSuppressed :: Tag -> Bool
tagSuppressed (NoTag) = True
tagSuppressed (UriTag "") = True
tagSuppressed _ = False

type AnchorName = String
type Anchor = Maybe AnchorName

tagToString :: Tag -> String
tagToString StrTag = "tag:yaml.org,2002:str"
tagToString FloatTag = "tag:yaml.org,2002:float"
tagToString NullTag = "tag:yaml.org,2002:null"
tagToString BoolTag = "tag:yaml.org,2002:bool"
tagToString SetTag = "tag:yaml.org,2002:set"
tagToString IntTag = "tag:yaml.org,2002:int"
tagToString SeqTag = "tag:yaml.org,2002:seq"
tagToString MapTag = "tag:yaml.org,2002:map"
tagToString (UriTag s) = s
tagToString NoTag = ""

bsToTag :: ByteString -> Tag
bsToTag = stringToTag . B8.unpack

stringToTag :: String -> Tag
stringToTag "tag:yaml.org,2002:str" = StrTag
stringToTag "tag:yaml.org,2002:float" = FloatTag
stringToTag "tag:yaml.org,2002:null" = NullTag
stringToTag "tag:yaml.org,2002:bool" = BoolTag
stringToTag "tag:yaml.org,2002:set" = SetTag
stringToTag "tag:yaml.org,2002:int" = IntTag
stringToTag "tag:yaml.org,2002:seq" = SeqTag
stringToTag "tag:yaml.org,2002:map" = MapTag
stringToTag "" = NoTag
stringToTag s = UriTag s

data ParserStruct
type Parser = Ptr ParserStruct
parserSize :: Int
parserSize = 480

data EventRawStruct
type EventRaw = Ptr EventRawStruct
eventSize :: Int
eventSize = 104

foreign import ccall unsafe "yaml_parser_initialize"
    c_yaml_parser_initialize :: Parser -> IO CInt

foreign import ccall unsafe "yaml_parser_delete"
    c_yaml_parser_delete :: Parser -> IO ()

foreign import ccall unsafe "yaml_parser_set_input_string"
    c_yaml_parser_set_input_string :: Parser
                                   -> Ptr CUChar
                                   -> CULong
                                   -> IO ()

foreign import ccall unsafe "yaml_parser_set_input_file"
    c_yaml_parser_set_input_file :: Parser
                                 -> File
                                 -> IO ()

data MarkRawStruct
type MarkRaw = Ptr MarkRawStruct

foreign import ccall unsafe "get_mark_index"
    c_get_mark_index :: MarkRaw -> IO CULong

foreign import ccall unsafe "get_mark_line"
    c_get_mark_line :: MarkRaw -> IO CULong

foreign import ccall unsafe "get_mark_column"
    c_get_mark_column :: MarkRaw -> IO CULong

getMark :: MarkRaw -> IO YamlMark
getMark m = YamlMark
  <$> (fromIntegral <$> c_get_mark_index m)
  <*> (fromIntegral <$> c_get_mark_line m)
  <*> (fromIntegral <$> c_get_mark_column m)

data FileStruct
type File = Ptr FileStruct

#ifdef WINDOWS
foreign import ccall unsafe "_fdopen"
#else
foreign import ccall unsafe "fdopen"
#endif
    c_fdopen :: CInt
             -> Ptr CChar
             -> IO File
foreign import ccall unsafe "fclose"
    c_fclose :: File
             -> IO ()

foreign import ccall unsafe "fclose_helper"
    c_fclose_helper :: File -> IO ()

foreign import ccall unsafe "yaml_parser_parse"
    c_yaml_parser_parse :: Parser -> EventRaw -> IO CInt

foreign import ccall unsafe "yaml_event_delete"
    c_yaml_event_delete :: EventRaw -> IO ()

foreign import ccall "get_parser_error_problem"
    c_get_parser_error_problem :: Parser -> IO (Ptr CUChar)

foreign import ccall "get_parser_error_context"
    c_get_parser_error_context :: Parser -> IO (Ptr CUChar)

foreign import ccall unsafe "get_parser_error_mark"
    c_get_parser_error_mark :: Parser -> IO MarkRaw

makeString :: MonadIO m => (a -> m (Ptr CUChar)) -> a -> m String
makeString f a = do
    cchar <- castPtr `liftM` f a
    if cchar == nullPtr
        then return ""
        else liftIO $ peekCString cchar

data EventType = YamlNoEvent
               | YamlStreamStartEvent
               | YamlStreamEndEvent
               | YamlDocumentStartEvent
               | YamlDocumentEndEvent
               | YamlAliasEvent
               | YamlScalarEvent
               | YamlSequenceStartEvent
               | YamlSequenceEndEvent
               | YamlMappingStartEvent
               | YamlMappingEndEvent
               deriving (Enum,Show)

foreign import ccall unsafe "get_event_type"
    c_get_event_type :: EventRaw -> IO CInt

foreign import ccall unsafe "get_start_mark"
    c_get_start_mark :: EventRaw -> IO MarkRaw

foreign import ccall unsafe "get_end_mark"
    c_get_end_mark :: EventRaw -> IO MarkRaw

foreign import ccall unsafe "get_scalar_value"
    c_get_scalar_value :: EventRaw -> IO (Ptr CUChar)

foreign import ccall unsafe "get_scalar_length"
    c_get_scalar_length :: EventRaw -> IO CULong

foreign import ccall unsafe "get_scalar_tag"
    c_get_scalar_tag :: EventRaw -> IO (Ptr CUChar)

foreign import ccall unsafe "get_scalar_style"
    c_get_scalar_style :: EventRaw -> IO CInt

foreign import ccall unsafe "get_scalar_anchor"
    c_get_scalar_anchor :: EventRaw -> IO CString

foreign import ccall unsafe "get_sequence_start_anchor"
    c_get_sequence_start_anchor :: EventRaw -> IO CString

foreign import ccall unsafe "get_sequence_start_style"
    c_get_sequence_start_style :: EventRaw -> IO CInt

foreign import ccall unsafe "get_sequence_start_tag"
    c_get_sequence_start_tag :: EventRaw -> IO (Ptr CUChar)

foreign import ccall unsafe "get_mapping_start_anchor"
    c_get_mapping_start_anchor :: EventRaw -> IO CString

foreign import ccall unsafe "get_mapping_start_style"
    c_get_mapping_start_style :: EventRaw -> IO CInt

foreign import ccall unsafe "get_mapping_start_tag"
    c_get_mapping_start_tag :: EventRaw -> IO (Ptr CUChar)

foreign import ccall unsafe "get_alias_anchor"
    c_get_alias_anchor :: EventRaw -> IO CString

readAnchor :: (EventRaw -> IO CString) -> EventRaw -> IO Anchor
readAnchor getAnchor er = do
  yanchor <- getAnchor er
  if yanchor == nullPtr
    then return Nothing
    else Just <$> peekCString yanchor

readStyle :: (Enum a) => (EventRaw -> IO CInt) -> EventRaw -> IO a
readStyle getStyle er = toEnum . fromEnum <$> getStyle er 

readTag :: (EventRaw -> IO (Ptr CUChar)) -> EventRaw -> IO Tag
readTag getTag er = bsToTag <$> (getTag er >>= packCString . castPtr) 

getEvent :: EventRaw -> IO (Maybe MarkedEvent)
getEvent er = do
    et <- c_get_event_type er
    startMark <- c_get_start_mark er >>= getMark
    endMark <- c_get_end_mark er >>= getMark
    event <- case toEnum $ fromEnum et of
        YamlNoEvent -> return Nothing
        YamlStreamStartEvent -> return $ Just EventStreamStart
        YamlStreamEndEvent -> return $ Just EventStreamEnd
        YamlDocumentStartEvent -> return $ Just EventDocumentStart
        YamlDocumentEndEvent -> return $ Just EventDocumentEnd
        YamlAliasEvent -> do
            yanchor <- c_get_alias_anchor er
            anchor <- if yanchor == nullPtr
                          then error "got YamlAliasEvent with empty anchor"
                          else peekCString yanchor
            return $ Just $ EventAlias anchor
        YamlScalarEvent -> do
            yvalue <- c_get_scalar_value er
            ylen <- c_get_scalar_length er
            let yvalue' = castPtr yvalue
            let ylen' = fromEnum ylen
            bs <- packCStringLen (yvalue', ylen')
            tag <- readTag c_get_scalar_tag er
            style <- readStyle c_get_scalar_style er
            anchor <- readAnchor c_get_scalar_anchor er
            return $ Just $ EventScalar bs tag style anchor
        YamlSequenceStartEvent -> do
            tag <- readTag c_get_sequence_start_tag er
            style <- readStyle c_get_sequence_start_style er
            anchor <- readAnchor c_get_sequence_start_anchor er
            return $ Just $ EventSequenceStart tag style anchor
        YamlSequenceEndEvent -> return $ Just EventSequenceEnd
        YamlMappingStartEvent -> do
            tag <- readTag c_get_mapping_start_tag er
            style <- readStyle c_get_mapping_start_style er
            anchor <- readAnchor c_get_mapping_start_anchor er
            return $ Just $ EventMappingStart tag style anchor
        YamlMappingEndEvent -> return $ Just EventMappingEnd
    return $ (\e -> MarkedEvent e startMark endMark) <$> event

-- Emitter

data EmitterStruct
type Emitter = Ptr EmitterStruct
emitterSize :: Int
emitterSize = 432

foreign import ccall unsafe "yaml_emitter_initialize"
    c_yaml_emitter_initialize :: Emitter -> IO CInt

foreign import ccall unsafe "yaml_emitter_delete"
    c_yaml_emitter_delete :: Emitter -> IO ()

data BufferStruct
type Buffer = Ptr BufferStruct
bufferSize :: Int
bufferSize = 16

foreign import ccall unsafe "buffer_init"
    c_buffer_init :: Buffer -> IO ()

foreign import ccall unsafe "get_buffer_buff"
    c_get_buffer_buff :: Buffer -> IO (Ptr CUChar)

foreign import ccall unsafe "get_buffer_used"
    c_get_buffer_used :: Buffer -> IO CULong

foreign import ccall unsafe "my_emitter_set_output"
    c_my_emitter_set_output :: Emitter -> Buffer -> IO ()

#ifndef __NO_UNICODE__
foreign import ccall unsafe "yaml_emitter_set_unicode"
    c_yaml_emitter_set_unicode :: Emitter -> CInt -> IO ()
#endif

foreign import ccall unsafe "yaml_emitter_set_output_file"
    c_yaml_emitter_set_output_file :: Emitter -> File -> IO ()

foreign import ccall unsafe "yaml_emitter_set_width"
    c_yaml_emitter_set_width :: Emitter -> CInt -> IO ()

foreign import ccall unsafe "yaml_emitter_emit"
    c_yaml_emitter_emit :: Emitter -> EventRaw -> IO CInt

foreign import ccall unsafe "yaml_stream_start_event_initialize"
    c_yaml_stream_start_event_initialize :: EventRaw -> CInt -> IO CInt

foreign import ccall unsafe "yaml_stream_end_event_initialize"
    c_yaml_stream_end_event_initialize :: EventRaw -> IO CInt

foreign import ccall unsafe "yaml_scalar_event_initialize"
    c_yaml_scalar_event_initialize
        :: EventRaw
        -> Ptr CUChar -- anchor
        -> Ptr CUChar -- tag
        -> Ptr CUChar -- value
        -> CInt       -- length
        -> CInt       -- plain_implicit
        -> CInt       -- quoted_implicit
        -> CInt       -- style
        -> IO CInt

foreign import ccall unsafe "simple_document_start"
    c_simple_document_start :: EventRaw -> IO CInt

foreign import ccall unsafe "yaml_document_end_event_initialize"
    c_yaml_document_end_event_initialize :: EventRaw -> CInt -> IO CInt

foreign import ccall unsafe "yaml_sequence_start_event_initialize"
    c_yaml_sequence_start_event_initialize
        :: EventRaw
        -> Ptr CUChar
        -> Ptr CUChar
        -> CInt
        -> CInt
        -> IO CInt

foreign import ccall unsafe "yaml_sequence_end_event_initialize"
    c_yaml_sequence_end_event_initialize :: EventRaw -> IO CInt

foreign import ccall unsafe "yaml_mapping_start_event_initialize"
    c_yaml_mapping_start_event_initialize
        :: EventRaw
        -> Ptr CUChar
        -> Ptr CUChar
        -> CInt
        -> CInt
        -> IO CInt

foreign import ccall unsafe "yaml_mapping_end_event_initialize"
    c_yaml_mapping_end_event_initialize :: EventRaw -> IO CInt

foreign import ccall unsafe "yaml_alias_event_initialize"
    c_yaml_alias_event_initialize
        :: EventRaw
        -> Ptr CUChar
        -> IO CInt

toEventRaw :: FormatOptions -> Event -> (EventRaw -> IO a) -> IO a
toEventRaw opts e f = allocaBytes eventSize $ \er -> do
    ret <- case e of
        EventStreamStart ->
            c_yaml_stream_start_event_initialize
                er
                0 -- YAML_ANY_ENCODING
        EventStreamEnd ->
            c_yaml_stream_end_event_initialize er
        EventDocumentStart ->
            c_simple_document_start er
        EventDocumentEnd ->
            c_yaml_document_end_event_initialize er 1
        EventScalar bs thetag style0 anchor -> do
            BU.unsafeUseAsCStringLen bs $ \(value, len) -> do
                let value' = castPtr value :: Ptr CUChar
                    len' = fromIntegral len :: CInt
                let thetag' = tagToString thetag
                withCString thetag' $ \tag' -> do
                    let pi0 = tagsImplicit e
                        (pi, style) =
                          case style0 of
                            PlainNoTag -> (1, Plain)
                            x -> (pi0, x)
                        style' = toEnum $ fromEnum style
                        tagP = castPtr tag'
                    case anchor of
                        Nothing ->
                            c_yaml_scalar_event_initialize
                                er
                                nullPtr -- anchor
                                tagP    -- tag
                                value'  -- value
                                len'    -- length
                                pi      -- plain_implicit
                                pi      -- quoted_implicit
                                style'  -- style
                        Just anchor' ->
                            withCString anchor' $ \anchorP' -> do
                                let anchorP = castPtr anchorP'
                                c_yaml_scalar_event_initialize
                                    er
                                    anchorP -- anchor
                                    tagP    -- tag
                                    value'  -- value
                                    len'    -- length
                                    0       -- plain_implicit
                                    pi      -- quoted_implicit
                                    style'  -- style
        EventSequenceStart tag style Nothing ->
            withCString (tagToString tag) $ \tag' -> do
                let tagP = castPtr tag'
                c_yaml_sequence_start_event_initialize
                  er
                  nullPtr
                  tagP
                  (tagsImplicit e)
                  (toEnum $ fromEnum style)
        EventSequenceStart tag style (Just anchor) ->
            withCString (tagToString tag) $ \tag' -> do
                let tagP = castPtr tag'
                withCString anchor $ \anchor' -> do
                    let anchorP = castPtr anchor'
                    c_yaml_sequence_start_event_initialize
                        er
                        anchorP
                        tagP
                        (tagsImplicit e)
                        (toEnum $ fromEnum style)
        EventSequenceEnd ->
            c_yaml_sequence_end_event_initialize er
        EventMappingStart tag style Nothing ->
            withCString (tagToString tag) $ \tag' -> do
                let tagP = castPtr tag'
                c_yaml_mapping_start_event_initialize
                    er
                    nullPtr
                    tagP
                    (tagsImplicit e)
                    (toEnum $ fromEnum style)
        EventMappingStart tag style (Just anchor) ->
            withCString (tagToString tag) $ \tag' -> do
                withCString anchor $ \anchor' -> do
                    let tagP = castPtr tag'
                    let anchorP = castPtr anchor'
                    c_yaml_mapping_start_event_initialize
                        er
                        anchorP
                        tagP
                        (tagsImplicit e)
                        (toEnum $ fromEnum style)
        EventMappingEnd ->
            c_yaml_mapping_end_event_initialize er
        EventAlias anchor ->
            withCString anchor $ \anchorP' -> do
                let anchorP = castPtr anchorP'
                c_yaml_alias_event_initialize
                    er
                    anchorP
    unless (ret == 1) $ throwIO $ ToEventRawException ret
    f er
  where
    tagsImplicit (EventScalar _ t _ _) | tagSuppressed t = 1
    tagsImplicit (EventMappingStart t _ _) | tagSuppressed t = 1
    tagsImplicit (EventSequenceStart t _ _) | tagSuppressed t = 1
    tagsImplicit evt = toImplicitParam $ formatOptionsRenderTags opts evt

newtype ToEventRawException = ToEventRawException CInt
    deriving (Show, Typeable)
instance Exception ToEventRawException

-- | Create a conduit that yields events from a bytestring.
decode :: MonadResource m => B.ByteString -> ConduitM i Event m ()
decode = mapOutput yamlEvent . decodeMarked

-- | Create a conduit that yields marked events from a bytestring.
--
-- This conduit will yield identical events to that of "decode", but also
-- includes start and end marks for each event.
--
-- @since 0.10.4.0
decodeMarked :: MonadResource m => B.ByteString -> ConduitM i MarkedEvent m ()
decodeMarked bs | B8.null bs = return ()
decodeMarked bs =
    bracketP alloc cleanup (runParser . fst)
  where
    alloc = mask_ $ do
        ptr <- mallocBytes parserSize
        res <- c_yaml_parser_initialize ptr
        if res == 0
            then do
                c_yaml_parser_delete ptr
                free ptr
                throwIO $ YamlException "Yaml out of memory"
            else do
                let (bsfptr, offset, len) = B.toForeignPtr bs
                let bsptrOrig = unsafeForeignPtrToPtr bsfptr
                let bsptr = castPtr bsptrOrig `plusPtr` offset
                c_yaml_parser_set_input_string ptr bsptr (fromIntegral len)
                return (ptr, bsfptr)
    cleanup (ptr, bsfptr) = do
        touchForeignPtr bsfptr
        c_yaml_parser_delete ptr
        free ptr

-- XXX copied from GHC.IO.FD
std_flags, read_flags, output_flags, write_flags :: CInt
std_flags    = Posix.o_NOCTTY
output_flags = std_flags    .|. Posix.o_CREAT .|. Posix.o_TRUNC
read_flags   = std_flags    .|. Posix.o_RDONLY
write_flags  = output_flags .|. Posix.o_WRONLY

-- | Open a C FILE* from a file path, using internal GHC API to work correctly
-- on all platforms, even on non-ASCII filenames. The opening mode must be
-- indicated via both 'rawOpenFlags' and 'openMode'.
openFile :: FilePath -> CInt -> String -> IO File
openFile file rawOpenFlags openMode = do
  fd <- liftIO $ Posix.withFilePath file $ \file' ->
    Posix.c_open file' rawOpenFlags 0o666
  if fd /= (-1)
    then withCString openMode $ \openMode' -> c_fdopen fd openMode'
    else return nullPtr

-- | Creata a conduit that yields events from a file.
decodeFile :: MonadResource m => FilePath -> ConduitM i Event m ()
decodeFile = mapOutput yamlEvent . decodeFileMarked

-- | Create a conduit that yields marked events from a file.
--
-- This conduit will yield identical events to that of "decodeFile", but also
-- includes start and end marks for each event.
--
-- @since 0.10.4.0
decodeFileMarked :: MonadResource m => FilePath -> ConduitM i MarkedEvent m ()
decodeFileMarked file =
    bracketP alloc cleanup (runParser . fst)
  where
    alloc = mask_ $ do
        ptr <- mallocBytes parserSize
        res <- c_yaml_parser_initialize ptr
        if res == 0
            then do
                c_yaml_parser_delete ptr
                free ptr
                throwIO $ YamlException "Yaml out of memory"
            else do
                file' <- openFile file read_flags "r"
                if file' == nullPtr
                    then do
                        c_yaml_parser_delete ptr
                        free ptr
                        throwIO $ YamlException
                                $ "Yaml file not found: " ++ file
                    else do
                        c_yaml_parser_set_input_file ptr file'
                        return (ptr, file')
    cleanup (ptr, file') = do
        c_fclose_helper file'
        c_yaml_parser_delete ptr
        free ptr

runParser :: MonadResource m => Parser -> ConduitM i MarkedEvent m ()
runParser parser = do
    e <- liftIO $ parserParseOne' parser
    case e of
        Left err -> liftIO $ throwIO err
        Right Nothing -> return ()
        Right (Just ev) -> yield ev >> runParser parser

parserParseOne' :: Parser
                -> IO (Either YamlException (Maybe MarkedEvent))
parserParseOne' parser = allocaBytes eventSize $ \er -> do
    res <- liftIO $ c_yaml_parser_parse parser er
    flip finally (c_yaml_event_delete er) $
      if res == 0
        then do
          problem <- makeString c_get_parser_error_problem parser
          context <- makeString c_get_parser_error_context parser
          problemMark <- c_get_parser_error_mark parser >>= getMark
          return $ Left $ YamlParseException problem context problemMark
        else Right <$> getEvent er

-- | Whether a tag should be rendered explicitly in the output or left
-- implicit.
--
-- @since 0.1.1.0
data TagRender = Explicit | Implicit
  deriving (Enum)

toImplicitParam :: TagRender -> CInt
toImplicitParam Explicit = 0
toImplicitParam Implicit = 1

-- | A value for 'formatOptionsRenderTags' that renders no
-- collection tags but all scalar tags (unless suppressed with styles
-- 'NoTag or 'PlainNoTag').
--
-- @since 0.1.1.0
renderScalarTags :: Event -> TagRender
renderScalarTags (EventScalar _ _ _ _) = Explicit
renderScalarTags (EventSequenceStart _ _ _) = Implicit
renderScalarTags (EventMappingStart _ _ _) = Implicit
renderScalarTags _ = Implicit

-- | A value for 'formatOptionsRenderTags' that renders all
-- tags (except 'NoTag' tag and 'PlainNoTag' style).
--
-- @since 0.1.1.0
renderAllTags :: Event -> TagRender
renderAllTags _ = Explicit

-- | A value for 'formatOptionsRenderTags' that renders no
-- tags.
--
-- @since 0.1.1.0
renderNoTags :: Event -> TagRender
renderNoTags _ = Implicit

-- | A value for 'formatOptionsRenderCollectionTags' that renders tags
-- which are instances of 'UriTag'
--
-- @since 0.1.1.0
renderUriTags :: Event -> TagRender
renderUriTags (EventScalar _ UriTag{} _ _) = Explicit
renderUriTags (EventSequenceStart UriTag{} _ _) = Explicit
renderUriTags (EventMappingStart UriTag{} _ _) = Explicit
renderUriTags _ = Implicit

-- | Contains options relating to the formatting (indendation, width) of the YAML output.
--
-- @since 0.10.2.0
data FormatOptions = FormatOptions
    { formatOptionsWidth :: Maybe Int
    , formatOptionsRenderTags :: Event -> TagRender
    }

-- |
-- @since 0.10.2.0
defaultFormatOptions :: FormatOptions
defaultFormatOptions = FormatOptions
    { formatOptionsWidth = Just 80 -- by default the width is set to 0 in the C code, which gets turned into 80 in yaml_emitter_emit_stream_start
    , formatOptionsRenderTags = renderScalarTags
    }

-- | Set the maximum number of columns in the YAML output, or 'Nothing' for infinite. By default, the limit is 80 characters.
--
-- @since 0.10.2.0
setWidth :: Maybe Int -> FormatOptions -> FormatOptions
setWidth w opts = opts { formatOptionsWidth = w }

-- | Control when and whether tags are rendered to output.
--
-- @since 0.1.1.0
setTagRendering :: (Event -> TagRender) -> FormatOptions -> FormatOptions
setTagRendering f opts = opts { formatOptionsRenderTags = f }

encode :: MonadResource m => ConduitM Event o m ByteString
encode = encodeWith defaultFormatOptions

-- |
-- @since 0.10.2.0
encodeWith :: MonadResource m => FormatOptions -> ConduitM Event o m ByteString
encodeWith opts =
    runEmitter opts alloc close
  where
    alloc emitter = do
        fbuf <- mallocForeignPtrBytes bufferSize
        withForeignPtr fbuf c_buffer_init
        withForeignPtr fbuf $ c_my_emitter_set_output emitter
        return fbuf
    close _ fbuf = withForeignPtr fbuf $ \b -> do
        ptr' <- c_get_buffer_buff b
        len <- c_get_buffer_used b
        fptr <- newForeignPtr_ $ castPtr ptr'
        return $ B.fromForeignPtr fptr 0 $ fromIntegral len


encodeFile :: MonadResource m
           => FilePath
           -> ConduitM Event o m ()
encodeFile = encodeFileWith defaultFormatOptions

-- |
-- @since 0.10.2.0
encodeFileWith :: MonadResource m
           => FormatOptions
           -> FilePath
           -> ConduitM Event o m ()
encodeFileWith opts filePath =
    bracketP getFile c_fclose $ \file -> runEmitter opts (alloc file) (\u _ -> return u)
  where
    getFile = do
        file <- openFile filePath write_flags "w"
        if file == nullPtr
            then throwIO $ YamlException $ "could not open file for write: " ++ filePath
            else return file

    alloc file emitter = c_yaml_emitter_set_output_file emitter file

runEmitter :: MonadResource m
           => FormatOptions
           -> (Emitter -> IO a) -- ^ alloc
           -> (() -> a -> IO b) -- ^ close
           -> ConduitM Event o m b
runEmitter opts allocI closeI =
    bracketP alloc cleanup go
  where
    alloc = mask_ $ do
        emitter <- mallocBytes emitterSize
        res <- c_yaml_emitter_initialize emitter
        when (res == 0) $ throwIO $ YamlException "c_yaml_emitter_initialize failed"
#ifndef __NO_UNICODE__
        c_yaml_emitter_set_unicode emitter 1
#endif
        c_yaml_emitter_set_width emitter $ case formatOptionsWidth opts of
            Nothing -> -1 --infinite
            Just width -> fromIntegral width
        a <- allocI emitter
        return (emitter, a)
    cleanup (emitter, _) = do
        c_yaml_emitter_delete emitter
        free emitter

    go (emitter, a) =
        loop
      where
        loop = await >>= maybe (close ()) push

        push e = do
            _ <- liftIO $ toEventRaw opts e $ c_yaml_emitter_emit emitter
            loop
        close u = liftIO $ closeI u a

-- | The pointer position
data YamlMark = YamlMark { yamlIndex :: Int, yamlLine :: Int, yamlColumn :: Int }
    deriving Show

data YamlException = YamlException String
                   -- | problem, context, index, position line, position column
                   | YamlParseException { yamlProblem :: String, yamlContext :: String, yamlProblemMark :: YamlMark }
    deriving (Show, Typeable)
instance Exception YamlException
