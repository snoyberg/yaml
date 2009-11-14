{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# INCLUDE <yaml.h> #-}
{-# INCLUDE <helper.h> #-}
module Text.Libyaml
{-
    (
      Event (..)
    , parserParse
    , withParser
    , emitEvents
    , withEmitter
    )-} where

import qualified Data.ByteString.Internal as B
import Control.Monad
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

import Control.Monad.Failure
import Data.Typeable (Typeable)
import Control.Exception (Exception)

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

withParser :: B.ByteString -> (Parser -> IO a) -> IO a
withParser bs f = allocaBytes parserSize $ \p ->
      do
        _res <- c_yaml_parser_initialize p
        -- FIXME check res
        let (fptr, offset, len) = B.toForeignPtr bs
        ret <- withForeignPtr fptr $ \ptr ->
                do
                    let ptr' = castPtr ptr `plusPtr` offset
                        len' = fromIntegral len
                    c_yaml_parser_set_input_string p ptr' len'
                    f p
        c_yaml_parser_delete p
        return ret

withEventRaw :: (EventRaw -> IO a) -> IO a
withEventRaw = allocaBytes eventSize

foreign import ccall unsafe "yaml_parser_parse"
    c_yaml_parser_parse :: Parser -> EventRaw -> IO CInt

foreign import ccall unsafe "yaml_event_delete"
    c_yaml_event_delete :: EventRaw -> IO ()

foreign import ccall "print_parser_error"
    c_print_parser_error :: Parser -> IO ()

data YamlException =
    YamlParserException String
    | YamlEmitterException String
    deriving (Show, Typeable)
instance Exception YamlException

parserParseOne :: MonadFailure YamlException m
               => Parser
               -> IO (m Event)
parserParseOne parser = withEventRaw $ \er -> do
    res <- c_yaml_parser_parse parser er
    event <-
        if res == 0
            then c_print_parser_error parser
                  >> return (failure $ YamlParserException "FIXME")
            else return `fmap` getEvent er
    c_yaml_event_delete er
    return event

data Event =
    EventNone
    | EventStreamStart
    | EventStreamEnd
    | EventDocumentStart
    | EventDocumentEnd
    | EventAlias
    | EventScalar B.ByteString
    | EventSequenceStart
    | EventSequenceEnd
    | EventMappingStart
    | EventMappingEnd
    deriving (Show)

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
            let yvalue' = castPtr yvalue
            let ylen' = fromEnum ylen
            let ylen'' = toEnum $ fromEnum ylen
            bs <- B.create ylen' $ \dest -> B.memcpy dest yvalue' ylen''
            return $ EventScalar bs
        YamlSequenceStartEvent -> return EventSequenceStart
        YamlSequenceEndEvent -> return EventSequenceEnd
        YamlMappingStartEvent -> return EventMappingStart
        YamlMappingEndEvent -> return EventMappingEnd

data YAttempt v = YSuccess v | YFailure YamlException -- FIXME use c-m-e?
instance Monad YAttempt where
    return = YSuccess
    YSuccess v >>= f = f v
    YFailure e >>= _ = YFailure e
instance MonadFailure YamlException YAttempt where
    failure = YFailure

parserParse :: MonadFailure YamlException m
            => Parser
            -> IO (m [Event])
parserParse parser = do
    event' <- parserParseOne parser
    case event' of
        YFailure e -> return $ failure e
        YSuccess event ->
            case event of
                EventStreamEnd -> return $ return [event]
                _ -> do
                    rest <- parserParse parser
                    case rest of
                        YFailure e -> return $ failure e
                        YSuccess rest' -> return $ return $ event : rest'

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

withBuffer :: MonadFailure YamlException m
           => (Buffer -> IO (YAttempt ()))
           -> IO (m B.ByteString)
withBuffer f = allocaBytes bufferSize $ \b -> do
        c_buffer_init b
        aRes <- f b
        case aRes of
            YFailure e -> return $ failure e
            YSuccess () -> do
                ptr' <- c_get_buffer_buff b
                len <- c_get_buffer_used b
                fptr <- newForeignPtr_ $ castPtr ptr'
                return $ return $ B.fromForeignPtr fptr 0 $ fromIntegral len

foreign import ccall unsafe "my_emitter_set_output"
    c_my_emitter_set_output :: Emitter -> Buffer -> IO ()

withEmitter :: MonadFailure YamlException m
            => (Emitter -> IO (YAttempt ()))
            -> IO (m B.ByteString)
withEmitter f = allocaBytes emitterSize $ \e -> do
        _res <- c_yaml_emitter_initialize e
        -- FIXME check res
        bs <- withBuffer $ \b -> do
                c_my_emitter_set_output e b
                f e
        c_yaml_emitter_delete e
        return bs

foreign import ccall unsafe "yaml_emitter_emit"
    c_yaml_emitter_emit :: Emitter -> EventRaw -> IO CInt

foreign import ccall unsafe "print_emitter_error"
    c_print_emitter_error :: Emitter -> IO ()

emitEvents :: MonadFailure YamlException m
           => Emitter
           -> [Event]
           -> IO (m ())
emitEvents _ [] = return $ return ()
emitEvents emitter (e:rest) = do
    res <- toEventRaw e $ c_yaml_emitter_emit emitter
    if res == 0
        then c_print_emitter_error emitter
                >> return (failure $ YamlEmitterException "FIXME")
        else emitEvents emitter rest

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
        -> CInt
        -> CInt
        -> CInt
        -> CInt
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
    case e of
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
        EventScalar bs -> do
            let (fvalue, offset, len) = B.toForeignPtr bs
            withForeignPtr fvalue $ \value -> do
                let value' = value `plusPtr` offset
                    len' = fromIntegral len
                    value'' = if ptrToIntPtr value' == 0
                                then intPtrToPtr 1 -- c/api.c:827
                                else value'
                c_yaml_scalar_event_initialize
                    er
                    nullPtr
                    nullPtr
                    value''
                    len'
                    0
                    1
                    0 -- YAML_ANY_SCALAR_STYLE
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
    f er
