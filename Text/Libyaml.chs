{-# LANGUAGE ForeignFunctionInterface #-}
module Text.Libyaml
    ( Event (..)
    , parserParse
    , withParser
    , emitEvents
    , withEmitter
    ) where

#include <yaml.h>
#include <helper.h>

import C2HS
import qualified Data.ByteString.Internal as B
import Control.Monad

{#context lib="yaml" #}

{#pointer *yaml_parser_t as Parser newtype#}
parserSize :: Int
parserSize = {#sizeof yaml_parser_t#}

{#pointer *yaml_event_t as EventRaw newtype#}
eventSize :: Int
eventSize = {#sizeof yaml_event_t#}

{#fun unsafe yaml_parser_initialize
    { id `Parser'
    } -> `Int' #}

{#fun unsafe yaml_parser_delete
    { id `Parser'
    } -> `()' #}

withParser :: B.ByteString -> (Parser -> IO a) -> IO a
withParser bs f = do
    allocaBytes parserSize $ \p ->
      do
        let p' = Parser p
        _res <- yaml_parser_initialize p'
        -- FIXME check res
        let (fptr, offset, len) = B.toForeignPtr bs
        ret <- withForeignPtr fptr $ \ptr ->
                do
                    let ptr' = castPtr ptr `plusPtr` offset
                    yaml_parser_set_input_string p' ptr' len
                    f p'
        yaml_parser_delete p'
        return ret

withEventRaw :: (EventRaw -> IO a) -> IO a
withEventRaw f = allocaBytes eventSize $ f . EventRaw

{#fun unsafe yaml_parser_set_input_string
    { id `Parser'
    , id `Ptr CUChar'
    , `Int'
    } -> `()' #}

{#fun unsafe yaml_parser_parse
    { id `Parser'
    , id `EventRaw'
    } -> `Int' #}

{#fun unsafe yaml_event_delete
    { id `EventRaw'
    } -> `()' #}

parserParse :: Parser -> IO [Event]
parserParse parser = do
    event <- parserParseOne parser
    case event of
		EventStreamEnd -> return [event]
		_ -> do
			rest <- parserParse parser
			return $! event : rest

{#fun unsafe print_parser_error
    { id `Parser'
    } -> `()' #}

parserParseOne :: Parser -> IO Event
parserParseOne parser = withEventRaw $ \er -> do
    res <- yaml_parser_parse parser er
    when (res == 0) $
        print_parser_error parser >> fail "yaml_parser_parse failed"
    event <- getEvent er
    yaml_event_delete er
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

{#enum yaml_event_type_e as EventType {underscoreToCase} deriving (Show) #}
getEvent :: EventRaw -> IO Event
getEvent (EventRaw ptr') = do
    et <- {#get yaml_event_t.type#} ptr'
    case toEnum $ fromEnum et of
		YamlNoEvent -> return EventNone
		YamlStreamStartEvent -> return EventStreamStart
		YamlStreamEndEvent -> return EventStreamEnd
		YamlDocumentStartEvent -> return EventDocumentStart
		YamlDocumentEndEvent -> return EventDocumentEnd
		YamlAliasEvent -> return EventAlias
		YamlScalarEvent -> do
			yvalue <- {#get yaml_event_t.data.scalar.value#} ptr'
			ylen <- {#get yaml_event_t.data.scalar.length#} ptr'
			let yvalue' = castPtr yvalue
			let ylen' = fromEnum ylen
			let ylen'' = toEnum $ fromEnum ylen
			bs <- B.create ylen' $ \dest -> B.memcpy dest yvalue' ylen''
			return $ EventScalar bs
		YamlSequenceStartEvent -> return EventSequenceStart
		YamlSequenceEndEvent -> return EventSequenceEnd
		YamlMappingStartEvent -> return EventMappingStart
		YamlMappingEndEvent -> return EventMappingEnd

-- Emitter
{#pointer *yaml_emitter_t as Emitter newtype#}
emitterSize :: Int
emitterSize = {#sizeof yaml_emitter_t#}

{#fun unsafe yaml_emitter_initialize
    { id `Emitter'
    } -> `Int' #}

{#fun unsafe yaml_emitter_delete
    { id `Emitter'
    } -> `()' #}

{#pointer *buffer_t as Buffer newtype#}
bufferSize :: Int
bufferSize = {#sizeof buffer_t#}

{#fun unsafe buffer_init
    { id `Buffer'
    } -> `()' #}

withBuffer :: (Buffer -> IO ()) -> IO B.ByteString
withBuffer f = do
    allocaBytes bufferSize $ \b -> do
        let b' = Buffer b
        buffer_init b'
        f b'
        ptr' <- {#get buffer_t.buff#} b
        len <- {#get buffer_t.used#} b
        fptr <- newForeignPtr_ $ castPtr ptr'
        return $! B.fromForeignPtr fptr 0 $ fromIntegral len

{#fun unsafe yaml_emitter_set_output
    { id `Emitter'
    , id `FunPtr (Ptr () -> Ptr CUChar -> CULong -> IO CInt)'
    , id `Ptr ()'
    } -> `()' #}

foreign import ccall "buffer.h &buffer_append"
    buffer_append :: FunPtr (Ptr () -> Ptr CUChar -> CULong -> IO CInt)

withEmitter :: (Emitter -> IO ()) -> IO B.ByteString
withEmitter f = do
    allocaBytes emitterSize $ \e -> do
        let e' = Emitter e
        _res <- yaml_emitter_initialize e'
        -- FIXME check res
        bs <- withBuffer $ \(Buffer b) -> do
                let fun = buffer_append
                yaml_emitter_set_output e' buffer_append $ castPtr b
                f e'
        yaml_emitter_delete e'
        return bs

{#fun unsafe yaml_emitter_emit
    { id `Emitter'
    , id `EventRaw'
    } -> `Int' #}

{#fun unsafe print_emitter_error
    { id `Emitter'
    } -> `()' #}

emitEvents :: Emitter -> [Event] -> IO ()
emitEvents _ [] = return ()
emitEvents emitter (e:rest) = do
    res <- toEventRaw e $ yaml_emitter_emit emitter
    when (res == 0) $
        print_emitter_error emitter >> fail "yaml_emitter_emit failed"
    emitEvents emitter rest

{#fun unsafe yaml_stream_start_event_initialize
    { id `EventRaw'
    , `Int'
    } -> `Int' #}

{#fun unsafe yaml_stream_end_event_initialize
    { id `EventRaw'
    } -> `Int' #}

{#fun unsafe yaml_scalar_event_initialize
    { id `EventRaw'
    , id `Ptr CUChar' -- anchor
    , id `Ptr CUChar' -- tag
    , id `Ptr CUChar' -- value
    , `Int'
    , `Int'
    , `Int'
    , `Int'
    } -> `Int' #}

{#fun unsafe simple_document_start
    { id `EventRaw'
    } -> `Int' #}

{#fun unsafe yaml_document_end_event_initialize
    { id `EventRaw'
    , `Int'
    } -> `Int' #}

{#fun unsafe yaml_sequence_start_event_initialize
    { id `EventRaw'
    , id `Ptr CUChar'
    , id `Ptr CUChar'
    , `Int'
    , `Int'
    } -> `Int' #}

{#fun unsafe yaml_sequence_end_event_initialize
    { id `EventRaw'
    } -> `Int' #}

{#fun unsafe yaml_mapping_start_event_initialize
    { id `EventRaw'
    , id `Ptr CUChar'
    , id `Ptr CUChar'
    , `Int'
    , `Int'
    } -> `Int' #}

{#fun unsafe yaml_mapping_end_event_initialize
    { id `EventRaw'
    } -> `Int' #}

toEventRaw :: Event -> (EventRaw -> IO a) -> IO a
toEventRaw e f = withEventRaw $ \er -> do
    case e of
        EventStreamStart ->
            yaml_stream_start_event_initialize
                er
                0 -- YAML_ANY_ENCODING
        EventStreamEnd ->
            yaml_stream_end_event_initialize er
        EventDocumentStart ->
            simple_document_start er
        EventDocumentEnd ->
            yaml_document_end_event_initialize er 1
        EventScalar bs -> do
            let (fvalue, offset, len) = B.toForeignPtr bs
            withForeignPtr fvalue $ \value -> do
                let value' = value `plusPtr` offset
                yaml_scalar_event_initialize
                    er
                    nullPtr
                    nullPtr
                    value'
                    len
                    0
                    1
                    0 -- YAML_ANY_SCALAR_STYLE
        EventSequenceStart ->
            yaml_sequence_start_event_initialize
                er
                nullPtr
                nullPtr
                1
                0 -- YAML_ANY_SEQUENCE_STYLE
        EventSequenceEnd ->
            yaml_sequence_end_event_initialize er
        EventMappingStart ->
            yaml_mapping_start_event_initialize
                er
                nullPtr
                nullPtr
                1
                0 -- YAML_ANY_SEQUENCE_STYLE
        EventMappingEnd ->
            yaml_mapping_end_event_initialize er
        EventAlias -> error "toEventRaw: EventAlias not supported"
        EventNone -> error "toEventRaw: EventNone not supported"
    f er
