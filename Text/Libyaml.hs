{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Libyaml
    ( -- * The event stream
      Event (..)
    , Style (..)
    , Tag (..)
      -- * Exceptions
    , YamlException (..)
      -- * Low level
    , withEmitter
    , emitEvents
    , withParser
    , parserParse
      -- * Enumerator
    , EnumResult (..)
      -- * Higher level functions
    , encode
    , decode
    , encodeFile
    , decodeFile
    ) where

import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString
import Data.ByteString (ByteString)
import Control.Monad
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

import Control.Exception (throwIO, Exception, SomeException)
import Data.Typeable (Typeable)

data Event =
    EventNone
    | EventStreamStart
    | EventStreamEnd
    | EventDocumentStart
    | EventDocumentEnd
    | EventAlias
    | EventScalar !ByteString !Tag !Style
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

data YamlException =
    YamlParserException
        { parserProblem :: String
        , parserContext :: String
        , parserOffset :: Int
        }
    | YamlEmitterException
        { emitterEvent :: Event
        , emitterProblem :: String
        }
    | YamlOutOfMemory
    | YamlInvalidEventStreamBeginning [Event]
    | YamlInvalidEventStreamEnd [Event]
    | YamlPrematureEventStreamEnd
    | YamlNonScalarKey
    | YamlInvalidStartingEvent Event
    | YamlFileNotFound FilePath
    | YamlOtherException SomeException
    deriving (Show, Typeable)
instance Exception YamlException

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

data FileStruct
type File = Ptr FileStruct

foreign import ccall unsafe "fopen"
    c_fopen :: Ptr CChar
            -> Ptr CChar
            -> IO File

foreign import ccall unsafe "fclose"
    c_fclose :: File
             -> IO ()

withFileParser :: FilePath
               -> (Parser -> IO (Either YamlException a))
               -> IO (Either YamlException a)
withFileParser fp f = allocaBytes parserSize $ \p ->
      do
        res <- c_yaml_parser_initialize p
        when (res == 0) $ throwIO YamlOutOfMemory
        file <- withCString fp $ \fp' -> withCString "r" $ \r' ->
                    c_fopen fp' r'
        when (file == nullPtr) $ throwIO $ YamlFileNotFound fp
        c_yaml_parser_set_input_file p file
        ret <- f p
        c_fclose file
        c_yaml_parser_delete p
        return ret

withParser :: B.ByteString
           -> (Parser -> IO (Either YamlException a))
           -> IO (Either YamlException a)
withParser bs f = allocaBytes parserSize $ \p -> do
  res <- c_yaml_parser_initialize p
  if res == 0
      then return (Left YamlOutOfMemory)
      else do
        let (fptr, offset, len) = B.toForeignPtr bs
        ret <- withForeignPtr fptr $ \ptr ->
                do
                    let ptr' = castPtr ptr `plusPtr` offset
                        len' = fromIntegral len
                    c_yaml_parser_set_input_string p ptr' len'
                    f p
        c_yaml_parser_delete p -- FIXME use finally
        return ret

withEventRaw :: (EventRaw -> IO a) -> IO a
withEventRaw = allocaBytes eventSize

foreign import ccall unsafe "yaml_parser_parse"
    c_yaml_parser_parse :: Parser -> EventRaw -> IO CInt

foreign import ccall unsafe "yaml_event_delete"
    c_yaml_event_delete :: EventRaw -> IO ()

foreign import ccall "get_parser_error_problem"
    c_get_parser_error_problem :: Parser -> IO (Ptr CUChar)

foreign import ccall "get_parser_error_context"
    c_get_parser_error_context :: Parser -> IO (Ptr CUChar)

foreign import ccall unsafe "get_parser_error_offset"
    c_get_parser_error_offset :: Parser -> IO CULong

makeString :: (a -> IO (Ptr CUChar)) -> a -> IO String
makeString f a = do
    cchar <- castPtr `fmap` f a
    peekCString cchar

parserParseOne :: Parser
               -> IO (Either YamlException Event)
parserParseOne parser = withEventRaw $ \er -> do
    res <- c_yaml_parser_parse parser er
    event <-
        if res == 0
            then do
                    problem <- makeString c_get_parser_error_problem parser
                    context <- makeString c_get_parser_error_context parser
                    offset <- fromIntegral `fmap`
                                c_get_parser_error_offset parser
                    return $ Left $
                        YamlParserException problem context offset
            else Right `fmap` getEvent er
    c_yaml_event_delete er
    return event

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

foreign import ccall unsafe "get_scalar_value"
    c_get_scalar_value :: EventRaw -> IO (Ptr CUChar)

foreign import ccall unsafe "get_scalar_length"
    c_get_scalar_length :: EventRaw -> IO CULong

foreign import ccall unsafe "get_scalar_tag"
    c_get_scalar_tag :: EventRaw -> IO (Ptr CUChar)

foreign import ccall unsafe "get_scalar_tag_len"
    c_get_scalar_tag_len :: EventRaw -> IO CULong

foreign import ccall unsafe "get_scalar_style"
    c_get_scalar_style :: EventRaw -> IO CInt

getEvent :: EventRaw -> IO Event
getEvent er = do
    et <- c_get_event_type er
    case toEnum $ fromEnum et of
        YamlNoEvent -> return EventNone
        YamlStreamStartEvent -> return EventStreamStart
        YamlStreamEndEvent -> return EventStreamEnd
        YamlDocumentStartEvent -> return EventDocumentStart
        YamlDocumentEndEvent -> return EventDocumentEnd
        YamlAliasEvent -> return EventAlias
        YamlScalarEvent -> do
            yvalue <- c_get_scalar_value er
            ylen <- c_get_scalar_length er
            ytag <- c_get_scalar_tag er
            ytag_len <- c_get_scalar_tag_len er
            ystyle <- c_get_scalar_style er
            let ytag_len' = fromEnum ytag_len
            let yvalue' = castPtr yvalue
            let ytag' = castPtr ytag
            let ylen' = fromEnum ylen
            let ylen'' = toEnum $ fromEnum ylen
            bs <- B.create ylen' $ \dest -> B.memcpy dest yvalue' ylen''
            tagbs <-
                if ytag_len' == 0
                    then return Data.ByteString.empty
                    else B.create ytag_len'
                      $ \dest -> B.memcpy dest ytag' (toEnum ytag_len')
            let style = toEnum $ fromEnum ystyle
            return $ EventScalar bs (bsToTag tagbs) style
        YamlSequenceStartEvent -> return EventSequenceStart
        YamlSequenceEndEvent -> return EventSequenceEnd
        YamlMappingStartEvent -> return EventMappingStart
        YamlMappingEndEvent -> return EventMappingEnd

parserParse :: (a -> Event -> IO (EnumResult a b))
            -> a
            -> Parser
            -> IO (Either YamlException b)
parserParse fold accum parser = do
    event' <- parserParseOne parser
    case event' of
        Left e -> return $ Left e
        Right event -> do
            res <- fold accum event
            case res of
                Done accum' -> return $ Right accum'
                More accum' -> parserParse fold accum' parser

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

withBuffer :: (Buffer -> IO (Either YamlException ()))
           -> IO (Either YamlException B.ByteString)
withBuffer f = allocaBytes bufferSize $ \b -> do
        c_buffer_init b
        aRes <- f b
        case aRes of
            Left e -> return $ Left e
            Right () -> do
                ptr' <- c_get_buffer_buff b
                len <- c_get_buffer_used b
                fptr <- newForeignPtr_ $ castPtr ptr'
                return $ Right $ B.fromForeignPtr fptr 0 $ fromIntegral len

foreign import ccall unsafe "my_emitter_set_output"
    c_my_emitter_set_output :: Emitter -> Buffer -> IO ()

withEmitter :: (Emitter -> IO (Either YamlException ()))
            -> IO (Either YamlException B.ByteString)
withEmitter f = allocaBytes emitterSize $ \e -> do
    res <- c_yaml_emitter_initialize e
    if res == 0
        then return $ Left YamlOutOfMemory
        else do
            bs <- withBuffer $ \b -> do
                    c_my_emitter_set_output e b
                    f e
            c_yaml_emitter_delete e
            return bs

foreign import ccall unsafe "yaml_emitter_set_output_file"
    c_yaml_emitter_set_output_file :: Emitter -> File -> IO ()

withEmitterFile :: FilePath
                -> (Emitter -> IO (Either YamlException ()))
                -> IO (Either YamlException ())
withEmitterFile fp f = allocaBytes emitterSize $ \e -> do
    res <- c_yaml_emitter_initialize e
    if res == 0
        then return $ Left YamlOutOfMemory
        else do
            file <- withCString fp $ \fp' -> withCString "w" $ \w' ->
                        c_fopen fp' w'
            res' <-
              if file == nullPtr
                then return $ Left $ YamlFileNotFound fp
                else do
                    c_yaml_emitter_set_output_file e file
                    res' <- f e
                    c_yaml_emitter_delete e
                    return res'
            c_fclose file
            return res'

foreign import ccall unsafe "yaml_emitter_emit"
    c_yaml_emitter_emit :: Emitter -> EventRaw -> IO CInt

foreign import ccall unsafe "get_emitter_error"
    c_get_emitter_error :: Emitter -> IO (Ptr CUChar)

emitEvents :: (a -> Maybe (Event, a))
           -> a
           -> Emitter
           -> IO (Either YamlException ())
emitEvents unfold src emitter = do
    case unfold src of
      Nothing -> return $ Right ()
      Just (e, src') -> do
        res <- toEventRaw e $ c_yaml_emitter_emit emitter
        if res == 0
            then do
                    problem <- makeString c_get_emitter_error emitter
                    return $ Left $ YamlEmitterException e problem
            else emitEvents unfold src' emitter

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

toEventRaw :: Event -> (EventRaw -> IO a) -> IO a
toEventRaw e f = withEventRaw $ \er -> do
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
        EventScalar bs thetag style -> do
            let (fvalue, offset, len) = B.toForeignPtr bs
            withForeignPtr fvalue $ \value -> do
                let value' = value `plusPtr` offset
                    len' = fromIntegral len
                    value'' = if ptrToIntPtr value' == 0
                                then intPtrToPtr 1 -- c/api.c:827
                                else value'
                let thetag' = tagToString thetag
                withCString thetag' $ \tag' -> do
                    let style' = toEnum $ fromEnum style
                        tagP = castPtr tag'
                        qi = if null thetag' then 1 else 0
                    c_yaml_scalar_event_initialize
                        er
                        nullPtr -- anchor
                        tagP    -- tag
                        value'' -- value
                        len'    -- length
                        0       -- plain_implicit
                        qi      -- quoted_implicit
                        style'  -- style
        EventSequenceStart ->
            c_yaml_sequence_start_event_initialize
                er
                nullPtr
                nullPtr
                1
                0 -- YAML_ANY_SEQUENCE_STYLE
        EventSequenceEnd ->
            c_yaml_sequence_end_event_initialize er
        EventMappingStart ->
            c_yaml_mapping_start_event_initialize
                er
                nullPtr
                nullPtr
                1
                0 -- YAML_ANY_SEQUENCE_STYLE
        EventMappingEnd ->
            c_yaml_mapping_end_event_initialize er
        EventAlias -> error "toEventRaw: EventAlias not supported"
        EventNone -> error "toEventRaw: EventNone not supported"
    unless (ret == 1) $ throwIO $ ToEventRawException ret
    f er

newtype ToEventRawException = ToEventRawException CInt
    deriving (Show, Typeable)
instance Exception ToEventRawException

data EnumResult a b = More a | Done b
    deriving (Eq, Show)

encode :: (a -> Maybe (Event, a))
       -> a
       -> IO (Either YamlException B.ByteString)
encode unfold src = withEmitter $ emitEvents unfold src

encodeFile :: FilePath
           -> (a -> Maybe (Event, a))
           -> a
           -> IO (Either YamlException ())
encodeFile filePath unfold src =
    withEmitterFile filePath $ emitEvents unfold src

decode :: B.ByteString
       -> (a -> Event -> IO (EnumResult a b))
       -> a
       -> IO (Either YamlException b)
decode bs fold accum = withParser bs $ parserParse fold accum

decodeFile :: FilePath
           -> (a -> Event -> IO (EnumResult a b))
           -> a
           -> IO (Either YamlException b)
decodeFile fp fold accum = withFileParser fp $ parserParse fold accum
