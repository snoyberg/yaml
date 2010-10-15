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
      -- * Encoding and decoding
    , encode
    , decode
    , encodeFile
    , decodeFile
      -- * Exception
    , YamlException (..)
    ) where

import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString
import qualified Data.ByteString.Unsafe as BU
import Data.ByteString (ByteString, packCStringLen)
import Control.Monad
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Data.Data

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)

import Control.Exception (throwIO, Exception, finally)
import Data.Enumerator
import Control.Applicative

data Event =
      EventStreamStart
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
    deriving (Show, Read, Eq, Enum, Bounded, Ord, Data, Typeable)

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

foreign import ccall unsafe "&yaml_parser_delete"
    c_yaml_parser_delete :: FunPtr (Parser -> IO ())

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

foreign import ccall unsafe "&fclose_helper"
    c_fclose_helper :: FunPtr (File -> Parser -> IO ())

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

getEvent :: EventRaw -> IO (Maybe Event)
getEvent er = do
    et <- c_get_event_type er
    case toEnum $ fromEnum et of
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
            ytag <- c_get_scalar_tag er
            ytag_len <- c_get_scalar_tag_len er
            ystyle <- c_get_scalar_style er
            let ytag_len' = fromEnum ytag_len
            let yvalue' = castPtr yvalue
            let ytag' = castPtr ytag
            let ylen' = fromEnum ylen
            bs <- packCStringLen (yvalue', ylen')
            tagbs <-
                if ytag_len' == 0
                    then return Data.ByteString.empty
                    else packCStringLen (ytag', ytag_len')
            let style = toEnum $ fromEnum ystyle
            yanchor <- c_get_scalar_anchor er
            anchor <- if yanchor == nullPtr
                          then return Nothing
                          else fmap Just $ peekCString yanchor
            return $ Just $ EventScalar bs (bsToTag tagbs) style anchor
        YamlSequenceStartEvent -> do
            yanchor <- c_get_sequence_start_anchor er
            anchor <- if yanchor == nullPtr
                          then return Nothing
                          else fmap Just $ peekCString yanchor
            return $ Just $ EventSequenceStart anchor
        YamlSequenceEndEvent -> return $ Just EventSequenceEnd
        YamlMappingStartEvent -> do
            yanchor <- c_get_mapping_start_anchor er
            anchor <- if yanchor == nullPtr
                          then return Nothing
                          else fmap Just $ peekCString yanchor
            return $ Just $ EventMappingStart anchor
        YamlMappingEndEvent -> return $ Just EventMappingEnd

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

foreign import ccall unsafe "yaml_emitter_set_output_file"
    c_yaml_emitter_set_output_file :: Emitter -> File -> IO ()

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

toEventRaw :: Event -> (EventRaw -> IO a) -> IO a
toEventRaw e f = allocaBytes eventSize $ \er -> do
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
            BU.unsafeUseAsCStringLen bs $ \(value, len) -> do
                let value' = castPtr value :: Ptr CUChar
                    len' = fromIntegral len :: CInt
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
                                value'  -- value
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
                                    value'  -- value
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
    unless (ret == 1) $ throwIO $ ToEventRawException ret
    f er

newtype ToEventRawException = ToEventRawException CInt
    deriving (Show, Typeable)
instance Exception ToEventRawException

decode :: MonadIO m => B.ByteString -> Enumerator Event m a
decode bs i = do
    fp <- liftIO $ mallocForeignPtrBytes parserSize
    res <- liftIO $ withForeignPtr fp c_yaml_parser_initialize
    liftIO $ addForeignPtrFinalizer c_yaml_parser_delete fp
    a <-
      if (res == 0)
        then throwError $ YamlException "Yaml out of memory"
        else do -- NOTE: can't replace the following with unsafeUseAsCString
                -- since it must run in a MonadIO
            let (fptr, offset, len) = B.toForeignPtr bs
            withForeignPtr' fptr $ \ptr -> do
                let ptr' = castPtr ptr `plusPtr` offset
                    len' = fromIntegral len
                liftIO $ withForeignPtr fp $ \p ->
                    c_yaml_parser_set_input_string p ptr' len'
                runParser fp i
    return a

decodeFile :: MonadIO m => FilePath -> Enumerator Event m a
decodeFile file i = do
    fp <- liftIO $ mallocForeignPtrBytes parserSize
    liftIO $ addForeignPtrFinalizer c_yaml_parser_delete fp
    res <- liftIO $ withForeignPtr fp c_yaml_parser_initialize
    a <-
      if res == 0
        then throwError $ YamlException "Yaml out of memory"
        else do
            file' <- liftIO
                    $ withCString file $ \file' -> withCString "r" $ \r' ->
                            c_fopen file' r'
            liftIO $ addForeignPtrFinalizerEnv c_fclose_helper file' fp
            if (file' == nullPtr)
                then throwError $ YamlException
                            $ "Yaml file not found: " ++ file
                else do
                    liftIO $ withForeignPtr fp $ \p ->
                        c_yaml_parser_set_input_file p file'
                    a <- runParser fp i
                    return a
    return a

runParser :: MonadIO m
          => ForeignPtr ParserStruct
          -> Enumerator Event m a
runParser fp (Continue k) = do
    e <- liftIO $ withForeignPtr fp parserParseOne'
    case e of
        Left err -> throwError $ YamlException err
        Right Nothing -> continue k
        Right (Just ev) -> do
            step' <- lift $ runIteratee $ k $ Chunks [ev]
            runParser fp step'
runParser _ step = returnI step

parserParseOne' :: Parser
                -> IO (Either String (Maybe Event))
parserParseOne' parser = allocaBytes eventSize $ \er -> do
    res <- liftIO $ c_yaml_parser_parse parser er
    flip finally (c_yaml_event_delete er) $
      if res == 0
        then do
          problem <- makeString c_get_parser_error_problem parser
          context <- makeString c_get_parser_error_context parser
          offset <- c_get_parser_error_offset parser
          return $ Left $ concat
            [ "YAML parse error: "
            , problem
            , "\nContext: "
            , context
            , "\nOffset: "
            , show offset
            , "\n"
            ]
        else liftIO $ Right <$> getEvent er

encode :: MonadIO m => Iteratee Event m B.ByteString
encode = do
    fp <- liftIO $ mallocForeignPtrBytes emitterSize
    res <- liftIO $ withForeignPtr fp c_yaml_emitter_initialize
    when (res == 0) $ throwError
        $ YamlException "c_yaml_emitter_initialize failed"
    buf <- liftIO $ mallocForeignPtrBytes bufferSize
    liftIO $ withForeignPtr buf c_buffer_init
    liftIO $ withForeignPtr fp $
        \emitter -> withForeignPtr buf $
        \b -> c_my_emitter_set_output emitter b
    runEmitter (go buf) fp
  where
    go buf = withForeignPtr buf $ \b -> do
        ptr' <- c_get_buffer_buff b
        len <- c_get_buffer_used b
        fptr <- newForeignPtr_ $ castPtr ptr'
        return $ B.fromForeignPtr fptr 0 $ fromIntegral len

encodeFile :: MonadIO m
           => FilePath
           -> Iteratee Event m ()
encodeFile filePath = do
    fp <- liftIO $ mallocForeignPtrBytes emitterSize
    res <- liftIO $ withForeignPtr fp c_yaml_emitter_initialize
    when (res == 0) $ throwError
        $ YamlException "c_yaml_emitter_initialize failed"
    file <- liftIO $ withCString filePath $
                \filePath' -> withCString "w" $
                \w' -> c_fopen filePath' w'
    when (file == nullPtr) $ throwError
        $ YamlException $ "could not open file for write: " ++ filePath
    liftIO $ withForeignPtr fp $ flip c_yaml_emitter_set_output_file file
    runEmitter (c_fclose file) fp

runEmitter :: MonadIO m
           => IO a
           -> ForeignPtr EmitterStruct
           -> Iteratee Event m a
runEmitter close fp = continue go
  where
    go EOF = do
        liftIO $ withForeignPtr fp c_yaml_emitter_delete
        a <- liftIO close
        yield a EOF
    go (Chunks c) = do
        liftIO $ withForeignPtr fp $ \emitter ->
            mapM_ (\e -> toEventRaw e $ c_yaml_emitter_emit emitter) c
        continue go

data YamlException = YamlException String
    deriving (Show, Typeable)
instance Exception YamlException
