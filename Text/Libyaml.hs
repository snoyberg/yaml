{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Text.Libyaml
    ( -- * The event stream
      Event (..)
    , Style (..)
    , Tag (..)
    , AnchorName
    , Anchor
      -- * Exceptions
    , YamlException (..)
      -- * Enumerator
    , With (..)
      -- * Encoder
    , YamlEncoder
    , YamlDecoder
    , Parser
    , parseEvent
    , parserParseOne
    , emitEvent
      -- ** Combinators
    , emitStream
    , emitDocument
    , emitSequence
    , emitSequenceWithAnchor
    , emitMapping
    , emitMappingWithAnchor
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
#if MIN_VERSION_transformers(0,2,0)
import "transformers" Control.Monad.IO.Class
#else
import "transformers" Control.Monad.Trans
#endif
import Control.Monad.Failure.Transformers
import qualified Control.Monad.Trans.Error as ErrorT
import Control.Monad.Trans.Reader

import Control.Exception (throwIO, Exception, SomeException)
import Data.Typeable (Typeable)
import Data.Iteratee
import qualified Data.Iteratee.Base.StreamChunk as SC
import Control.Applicative
import Control.Monad.CatchIO (MonadCatchIO, finally)

data Event =
    EventNone
    | EventStreamStart
    | EventStreamEnd
    | EventDocumentStart
    | EventDocumentEnd
    | EventAlias !AnchorName
    | EventScalar !ByteString !Tag !Style !Anchor
    | EventSequenceStart !Anchor
    | EventSequenceEnd
    | EventMappingStart !Anchor
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
    | YamlStringException String
    deriving (Show, Typeable)
instance Exception YamlException
instance ErrorT.Error YamlException where
    strMsg = YamlStringException

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

class MonadIO m => With m where
    with :: (forall b'. (a -> IO b') -> IO b') -> (a -> m b) -> m b

instance With IO where
    with = id
instance With m => With (ReaderT r m) where
    with orig f = ReaderT $ \r -> do
        let f' a = (runReaderT $ f a) r
        with orig f'
instance (ErrorT.Error e, With m) => With (ErrorT.ErrorT e m) where
    with orig f = ErrorT.ErrorT $ with orig $ ErrorT.runErrorT . f

allocaBytesR :: With m => Int -> (Ptr a -> m b) -> m b
allocaBytesR i = with (allocaBytes i)

withCStringR :: With m => String -> (Ptr CChar -> m a) -> m a
withCStringR s = with (withCString s)

withForeignPtr' :: MonadIO m => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtr' fp f = do
    r <- f $ unsafeForeignPtrToPtr fp
    liftIO $ touchForeignPtr fp
    return r

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
    if cchar == nullPtr
        then return ""
        else liftIO $ peekCString cchar

parserParseOne :: (MonadFailure YamlException m, With m)
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

foreign import ccall unsafe "get_scalar_anchor"
    c_get_scalar_anchor :: EventRaw -> IO CString

foreign import ccall unsafe "get_sequence_start_anchor"
    c_get_sequence_start_anchor :: EventRaw -> IO CString

foreign import ccall unsafe "get_mapping_start_anchor"
    c_get_mapping_start_anchor :: EventRaw -> IO CString

foreign import ccall unsafe "get_alias_anchor"
    c_get_alias_anchor :: EventRaw -> IO CString

getEvent :: EventRaw -> IO Event
getEvent er = do
    et <- c_get_event_type er
    case toEnum $ fromEnum et of
        YamlNoEvent -> return EventNone
        YamlStreamStartEvent -> return EventStreamStart
        YamlStreamEndEvent -> return EventStreamEnd
        YamlDocumentStartEvent -> return EventDocumentStart
        YamlDocumentEndEvent -> return EventDocumentEnd
        YamlAliasEvent -> do
            yanchor <- c_get_alias_anchor er
            anchor <- if yanchor == nullPtr
                          then error "got YamlAliasEvent with empty anchor"
                          else peekCString yanchor
            return $ EventAlias anchor
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
            yanchor <- c_get_scalar_anchor er
            anchor <- if yanchor == nullPtr
                          then return Nothing
                          else fmap Just $ peekCString yanchor
            return $ EventScalar bs (bsToTag tagbs) style anchor
        YamlSequenceStartEvent -> do
            yanchor <- c_get_sequence_start_anchor er
            anchor <- if yanchor == nullPtr
                          then return Nothing
                          else fmap Just $ peekCString yanchor
            return $ EventSequenceStart anchor
        YamlSequenceEndEvent -> return EventSequenceEnd
        YamlMappingStartEvent -> do
            yanchor <- c_get_mapping_start_anchor er
            anchor <- if yanchor == nullPtr
                          then return Nothing
                          else fmap Just $ peekCString yanchor
            return $ EventMappingStart anchor
        YamlMappingEndEvent -> return EventMappingEnd

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

withBufferR :: (With m, MonadFailure YamlException m)
            => (Buffer -> m ())
            -> m B.ByteString
withBufferR f = allocaBytesR bufferSize $ \b -> do
    liftIO $ c_buffer_init b
    f b
    ptr' <- liftIO $ c_get_buffer_buff b
    len <- liftIO $ c_get_buffer_used b
    fptr <- liftIO $ newForeignPtr_ $ castPtr ptr'
    return $ B.fromForeignPtr fptr 0 $ fromIntegral len

foreign import ccall unsafe "my_emitter_set_output"
    c_my_emitter_set_output :: Emitter -> Buffer -> IO ()

withEmitter :: (With m, MonadFailure YamlException m)
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

withEmitterFile :: (With m, MonadFailure YamlException m)
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

emitEvent :: (MonadIO m, MonadFailure YamlException m)
          => Event
          -> YamlEncoder m ()
emitEvent e = do
    emitter <- ask
    res <- liftIO $ toEventRaw e $ c_yaml_emitter_emit emitter
    when (res == 0) $ do
        problem <- liftIO $ makeString c_get_emitter_error emitter
        failure $ YamlEmitterException e problem

emitStream, emitDocument, emitSequence, emitMapping
    :: (MonadIO m, MonadFailure YamlException m)
    => YamlEncoder m ()
    -> YamlEncoder m ()
emitStream e = emitEvent EventStreamStart >> e >> emitEvent EventStreamEnd
emitDocument e = emitEvent EventDocumentStart >> e
              >> emitEvent EventDocumentEnd
emitSequence e = emitSequenceWithAnchor e Nothing
emitMapping e = emitMappingWithAnchor e Nothing

emitSequenceWithAnchor, emitMappingWithAnchor
    :: (MonadIO m, MonadFailure YamlException m)
    => YamlEncoder m ()
    -> Anchor
    -> YamlEncoder m ()
emitSequenceWithAnchor e a = emitEvent (EventSequenceStart a) >> e
                          >> emitEvent EventSequenceEnd
emitMappingWithAnchor e a = emitEvent (EventMappingStart a) >> e
                         >> emitEvent EventMappingEnd


parseEvent :: (With m, MonadFailure YamlException m)
           => YamlDecoder m Event
parseEvent = ask >>= parserParseOne

type YamlDecoder = ReaderT Parser
type YamlEncoder = ReaderT Emitter

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
        EventScalar bs thetag style anchor -> do
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
                    case anchor of
                        Nothing ->
                            c_yaml_scalar_event_initialize
                                er
                                nullPtr -- anchor
                                tagP    -- tag
                                value'' -- value
                                len'    -- length
                                0       -- plain_implicit
                                qi      -- quoted_implicit
                                style'  -- style
                        Just anchor' ->
                            withCString anchor' $ \anchorP' -> do
                                let anchorP = castPtr anchorP'
                                c_yaml_scalar_event_initialize
                                    er
                                    anchorP -- anchor
                                    tagP    -- tag
                                    value'' -- value
                                    len'    -- length
                                    0       -- plain_implicit
                                    qi      -- quoted_implicit
                                    style'  -- style
        EventSequenceStart Nothing ->
            c_yaml_sequence_start_event_initialize
                er
                nullPtr
                nullPtr
                1
                0 -- YAML_ANY_SEQUENCE_STYLE
        EventSequenceStart (Just anchor) ->
            withCString anchor $ \anchor' -> do
                let anchorP = castPtr anchor'
                c_yaml_sequence_start_event_initialize
                    er
                    anchorP
                    nullPtr
                    1
                    0 -- YAML_ANY_SEQUENCE_STYLE
        EventSequenceEnd ->
            c_yaml_sequence_end_event_initialize er
        EventMappingStart Nothing ->
            c_yaml_mapping_start_event_initialize
                er
                nullPtr
                nullPtr
                1
                0 -- YAML_ANY_SEQUENCE_STYLE
        EventMappingStart (Just anchor) ->
            withCString anchor $ \anchor' -> do
                let anchorP = castPtr anchor'
                c_yaml_mapping_start_event_initialize
                    er
                    anchorP
                    nullPtr
                    1
                    0 -- YAML_ANY_SEQUENCE_STYLE
        EventMappingEnd ->
            c_yaml_mapping_end_event_initialize er
        EventAlias anchor ->
            withCString anchor $ \anchorP' -> do
                let anchorP = castPtr anchorP'
                c_yaml_alias_event_initialize
                    er
                    anchorP
        EventNone -> error "toEventRaw: EventNone not supported"
    unless (ret == 1) $ throwIO $ ToEventRawException ret
    f er

newtype ToEventRawException = ToEventRawException CInt
    deriving (Show, Typeable)
instance Exception ToEventRawException

encode :: (With m, MonadFailure YamlException m)
       => YamlEncoder m ()
       -> m B.ByteString
encode = withEmitter . runReaderT

encodeFile :: (With m, MonadFailure YamlException m)
           => FilePath
           -> YamlEncoder m ()
           -> m ()
encodeFile filePath = withEmitterFile filePath . runReaderT

decode :: SC.StreamChunk c Event
       => MonadCatchIO m
       => B.ByteString
       -> EnumeratorGM c Event m a
decode bs i = do
    fp <- liftIO $ mallocForeignPtrBytes parserSize
    res <- liftIO $ withForeignPtr fp c_yaml_parser_initialize
    flip finally (liftIO $ withForeignPtr fp c_yaml_parser_delete) $
      if (res == 0)
        then return $ throwErr $ Err "Yaml out of memory"
        else do
            let (fptr, offset, len) = B.toForeignPtr bs
            withForeignPtr' fptr $ \ptr -> do
                let ptr' = castPtr ptr `plusPtr` offset
                    len' = fromIntegral len
                liftIO $ withForeignPtr fp $ \p ->
                    c_yaml_parser_set_input_string p ptr' len'
                runParser fp i

decodeFile :: SC.StreamChunk c Event
           => MonadCatchIO m
           => FilePath
           -> EnumeratorGM c Event m a
decodeFile file i = do
    fp <- liftIO $ mallocForeignPtrBytes parserSize
    res <- liftIO $ withForeignPtr fp c_yaml_parser_initialize
    flip finally (liftIO $ withForeignPtr fp c_yaml_parser_delete) $
      if (res == 0)
        then return $ throwErr $ Err "Yaml out of memory"
        else do
            file' <- liftIO
                    $ withCStringR file $ \file' -> withCStringR "r" $ \r' ->
                            c_fopen file' r'
            if (file' == nullPtr)
                then return $ throwErr $ Err
                            $ "Yaml file not found: " ++ file
                else do
                    liftIO $ withForeignPtr fp $ \p ->
                        c_yaml_parser_set_input_file p file'
                    finally (runParser fp i) $ liftIO $ do
                        c_fclose file'
                        withForeignPtr fp c_yaml_parser_delete

runParser :: SC.StreamChunk c Event
          => MonadCatchIO m
          => ForeignPtr ParserStruct
          -> IterateeG c Event m a
          -> m (IterateeG c Event m a)
runParser fp iter = do
    e <- liftIO $ withForeignPtr fp parserParseOne'
    case e of
        Left err -> return $ throwErr $ Err $ show err
        Right EventNone -> return iter
        Right ev -> do
            igv <- runIter iter $ Chunk $ SC.fromList [ev]
            case igv of
                Done a _ -> return $ return a
                Cont iter' Nothing -> runParser fp iter'
                Cont _ (Just err) -> return $ throwErr err

parserParseOne' :: Parser
                -> IO (Either YamlException Event)
parserParseOne' parser = allocaBytesR eventSize $ \er -> do
    res <- liftIO $ c_yaml_parser_parse parser er
    flip finally (c_yaml_event_delete er) $
      if res == 0
        then do
          problem <- makeString c_get_parser_error_problem parser
          context <- makeString c_get_parser_error_context parser
          offset <- fromIntegral `fmap`
                                c_get_parser_error_offset parser
          return $ Left $ YamlParserException problem context offset
        else liftIO $ Right <$> getEvent er
