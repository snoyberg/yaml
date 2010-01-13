{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Libyaml
    ( -- * The event stream
      Event (..)
    , Style (..)
    , Tag (..)
      -- * Exceptions
    , YamlException (..)
      -- * Enumerator
    , EnumResult (..)
    , RMonadIO (..)
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
import "transformers" Control.Monad.Trans
import Control.Failure

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

class MonadIO m => RMonadIO m where
    type Inside :: * -> *
    ioOut :: m v -> IO (Inside v)
    ioIn :: IO (Inside v) -> m v
newtype SimpleIdent x = SimpleIdent { unSimpleIdent :: x }
instance RMonadIO IO where
    type Inside = SimpleIdent
    ioOut = fmap SimpleIdent
    ioIn = fmap unSimpleIdent

allocaBytesR :: RMonadIO m => Int -> (Ptr a -> m b) -> m b
allocaBytesR i f = ioIn $ allocaBytes i $ ioOut . f

withCStringR :: RMonadIO m => String -> (Ptr CChar -> m a) -> m a
withCStringR s f = ioIn $ withCString s $ ioOut . f

withFileParser :: (MonadFailure YamlException m, RMonadIO m)
               => FilePath
               -> (Parser -> m a)
               -> m a
withFileParser fp f = allocaBytesR parserSize $ \p -> do
    res <- liftIO $ c_yaml_parser_initialize p
    when (res == 0) $ failure YamlOutOfMemory
    file <- withCStringR fp $ \fp' -> withCStringR "r" $ \r' ->
                    liftIO (c_fopen fp' r')
    when (file == nullPtr) $ failure $ YamlFileNotFound fp
    liftIO $ c_yaml_parser_set_input_file p file
    ret <- f p
    liftIO $ c_fclose file
    liftIO $ c_yaml_parser_delete p -- could use some finallys here
    return ret

withParser :: (MonadFailure YamlException m, RMonadIO m)
           => B.ByteString
           -> (Parser -> m a)
           -> m a
withParser bs f = allocaBytesR parserSize $ \p -> do
    res <- liftIO $ c_yaml_parser_initialize p
    when (res == 0) $ failure YamlOutOfMemory
    let (fptr, offset, len) = B.toForeignPtr bs
    ret <- withForeignPtrR fptr $ \ptr -> do
             let ptr' = castPtr ptr `plusPtr` offset
                 len' = fromIntegral len
             liftIO $ c_yaml_parser_set_input_string p ptr' len'
             f p
    liftIO $ c_yaml_parser_delete p -- FIXME use finally
    return ret

withForeignPtrR :: RMonadIO m
                => ForeignPtr a
                -> (Ptr a -> m b)
                -> m b
withForeignPtrR fp f = ioIn $ withForeignPtr fp $ ioOut . f

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

makeString :: MonadIO m => (a -> m (Ptr CUChar)) -> a -> m String
makeString f a = do
    cchar <- castPtr `liftM` f a
    liftIO $ peekCString cchar

parserParseOne :: (MonadFailure YamlException m, RMonadIO m)
               => Parser
               -> m Event
parserParseOne parser = allocaBytesR eventSize $ \er -> do
    res <- liftIO $ c_yaml_parser_parse parser er
    event <-
      if res == 0
        then do
          problem <- liftIO $ makeString c_get_parser_error_problem parser
          context <- liftIO $ makeString c_get_parser_error_context parser
          offset <- liftIO $ fromIntegral `fmap`
                                c_get_parser_error_offset parser
          failure $ YamlParserException problem context offset
        else liftIO $ getEvent er
    liftIO $ c_yaml_event_delete er -- FIXME use finally
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

parserParse :: (MonadFailure YamlException m, RMonadIO m)
            => (a -> Event -> m (EnumResult a b))
            -> a
            -> Parser
            -> m b
parserParse fold accum parser = do
    event <- liftIO $ parserParseOne parser
    res <- fold accum event
    case res of
        Done accum' -> return accum'
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

withBufferR :: (RMonadIO m, MonadFailure YamlException m)
            => (Buffer -> m ())
            -> m B.ByteString
withBufferR f = allocaBytesR bufferSize $ \b -> do
    liftIO $ c_buffer_init b
    aRes <- f b
    ptr' <- liftIO $ c_get_buffer_buff b
    len <- liftIO $ c_get_buffer_used b
    fptr <- liftIO $ newForeignPtr_ $ castPtr ptr'
    return $ B.fromForeignPtr fptr 0 $ fromIntegral len

foreign import ccall unsafe "my_emitter_set_output"
    c_my_emitter_set_output :: Emitter -> Buffer -> IO ()

withEmitter :: (RMonadIO m, MonadFailure YamlException m)
            => (Emitter -> m ())
            -> m B.ByteString
withEmitter f = allocaBytesR emitterSize $ \e -> do
    res <- liftIO $ c_yaml_emitter_initialize e
    when (res == 0) $ failure YamlOutOfMemory
    bs <- withBufferR $ \b -> do
            liftIO $ c_my_emitter_set_output e b
            f e
    liftIO $ c_yaml_emitter_delete e -- FIXME finally
    return bs

foreign import ccall unsafe "yaml_emitter_set_output_file"
    c_yaml_emitter_set_output_file :: Emitter -> File -> IO ()

withEmitterFile :: (RMonadIO m, MonadFailure YamlException m)
                => FilePath
                -> (Emitter -> m ())
                -> m ()
withEmitterFile fp f = allocaBytesR emitterSize $ \e -> do
    res <- liftIO $ c_yaml_emitter_initialize e
    when (res == 0) $ failure YamlOutOfMemory
    file <- withCStringR fp $ \fp' -> withCStringR "w" $ \w' ->
                        liftIO (c_fopen fp' w')
    res' <-
        if file == nullPtr
            then failure $ YamlFileNotFound fp
            else do
                liftIO $ c_yaml_emitter_set_output_file e file
                res' <- f e
                liftIO $ c_yaml_emitter_delete e
                return res'
    liftIO $ c_fclose file -- FIXME use finally
    return res'

foreign import ccall unsafe "yaml_emitter_emit"
    c_yaml_emitter_emit :: Emitter -> EventRaw -> IO CInt

foreign import ccall unsafe "get_emitter_error"
    c_get_emitter_error :: Emitter -> IO (Ptr CUChar)

emitEvents :: (MonadFailure YamlException m, RMonadIO m)
           => (a -> Maybe (Event, a))
           -> a
           -> Emitter
           -> m ()
emitEvents unfold src emitter = case unfold src of
    Nothing -> return ()
    Just (e, src') -> do
        res <- liftIO $ toEventRaw e $ c_yaml_emitter_emit emitter
        when (res == 0) $ do
            problem <- liftIO $ makeString c_get_emitter_error emitter
            failure $ YamlEmitterException e problem
        emitEvents unfold src' emitter

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
toEventRaw e f = allocaBytesR eventSize $ \er -> do
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

encode :: (RMonadIO m, MonadFailure YamlException m)
       => (a -> Maybe (Event, a))
       -> a
       -> m B.ByteString
encode unfold src = withEmitter $ emitEvents unfold src

encodeFile :: (RMonadIO m, MonadFailure YamlException m)
           => FilePath
           -> (a -> Maybe (Event, a))
           -> a
           -> m ()
encodeFile filePath unfold src =
    withEmitterFile filePath $ emitEvents unfold src

decode :: (RMonadIO m, MonadFailure YamlException m)
       => B.ByteString
       -> (a -> Event -> m (EnumResult a b))
       -> a
       -> m b
decode bs fold accum = withParser bs $ parserParse fold accum

decodeFile :: (RMonadIO m, MonadFailure YamlException m)
           => FilePath
           -> (a -> Event -> m (EnumResult a b))
           -> a
           -> m b
decodeFile fp fold accum = withFileParser fp $ parserParse fold accum
