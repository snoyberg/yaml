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

import Data.ByteString (ByteString, packCStringLen)
import qualified Data.ByteString
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
           | PlainNoTag
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

foreign import ccall unsafe "get_parser_error_index"
    c_get_parser_error_index :: Parser -> IO CULong

foreign import ccall unsafe "get_parser_error_line"
    c_get_parser_error_line :: Parser -> IO CULong

foreign import ccall unsafe "get_parser_error_column"
    c_get_parser_error_column :: Parser -> IO CULong

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
                          else Just <$> peekCString yanchor
            return $ Just $ EventScalar bs (bsToTag tagbs) style anchor
        YamlSequenceStartEvent -> do
            yanchor <- c_get_sequence_start_anchor er
            anchor <- if yanchor == nullPtr
                          then return Nothing
                          else Just <$> peekCString yanchor
            return $ Just $ EventSequenceStart anchor
        YamlSequenceEndEvent -> return $ Just EventSequenceEnd
        YamlMappingStartEvent -> do
            yanchor <- c_get_mapping_start_anchor er
            anchor <- if yanchor == nullPtr
                          then return Nothing
                          else Just <$> peekCString yanchor
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

#ifndef __NO_UNICODE__
foreign import ccall unsafe "yaml_emitter_set_unicode"
    c_yaml_emitter_set_unicode :: Emitter -> CInt -> IO ()
#endif

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
        EventScalar bs thetag style0 anchor -> do
            BU.unsafeUseAsCStringLen bs $ \(value, len) -> do
                let value' = castPtr value :: Ptr CUChar
                    len' = fromIntegral len :: CInt
                let thetag' = tagToString thetag
                withCString thetag' $ \tag' -> do
                    let (pi, style) =
                            case style0 of
                                PlainNoTag -> (1, Plain)
                                x -> (0, x)
                        style' = toEnum $ fromEnum style
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
                                pi      -- plain_implicit
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

decode :: MonadResource m => B.ByteString
#if MIN_VERSION_conduit(1, 0, 0)
       -> Producer m Event
#else
       -> GSource m Event
#endif
decode bs | B8.null bs = return ()
decode bs =
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

decodeFile :: MonadResource m => FilePath
#if MIN_VERSION_conduit(1, 0, 0)
           -> Producer m Event
#else
           -> GSource m Event
#endif
decodeFile file =
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

runParser :: MonadResource m => Parser
#if MIN_VERSION_conduit(1, 0, 0)
           -> Producer m Event
#else
           -> GSource m Event
#endif
runParser parser = do
    e <- liftIO $ parserParseOne' parser
    case e of
        Left err -> liftIO $ throwIO err
        Right Nothing -> return ()
        Right (Just ev) -> yield ev >> runParser parser

parserParseOne' :: Parser
                -> IO (Either YamlException (Maybe Event))
parserParseOne' parser = allocaBytes eventSize $ \er -> do
    res <- liftIO $ c_yaml_parser_parse parser er
    flip finally (c_yaml_event_delete er) $
      if res == 0
        then do
          problem <- makeString c_get_parser_error_problem parser
          context <- makeString c_get_parser_error_context parser
          index <- c_get_parser_error_index parser
          line <- c_get_parser_error_line parser
          column <- c_get_parser_error_column parser
          let problemMark = YamlMark (fromIntegral index) (fromIntegral line) (fromIntegral column)
          return $ Left $ YamlParseException problem context problemMark
        else Right <$> getEvent er

encode :: MonadResource m
#if MIN_VERSION_conduit(1, 0, 0)
       => Consumer Event m ByteString
#else
       => GSink Event m ByteString
#endif
encode =
    runEmitter alloc close
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
#if MIN_VERSION_conduit(1, 0, 0)
           -> Consumer Event m ()
#else
           -> GInfSink Event m
#endif
encodeFile filePath =
    bracketP getFile c_fclose $ \file -> runEmitter (alloc file) (\u _ -> return u)
  where
    getFile = do
        file <- openFile filePath write_flags "w"
        if file == nullPtr
            then throwIO $ YamlException $ "could not open file for write: " ++ filePath
            else return file

    alloc file emitter = c_yaml_emitter_set_output_file emitter file

runEmitter :: MonadResource m
           => (Emitter -> IO a) -- ^ alloc
#if MIN_VERSION_conduit(1, 0, 0)
           -> (() -> a -> IO b) -- ^ close
           -> Consumer Event m b
#else
           -> (u -> a -> IO b) -- ^ close
           -> Pipe l Event o u m b
#endif
runEmitter allocI closeI =
    bracketP alloc cleanup go
  where
    alloc = mask_ $ do
        emitter <- mallocBytes emitterSize
        res <- c_yaml_emitter_initialize emitter
        when (res == 0) $ throwIO $ YamlException "c_yaml_emitter_initialize failed"
#ifndef __NO_UNICODE__
        c_yaml_emitter_set_unicode emitter 1
#endif
        a <- allocI emitter
        return (emitter, a)
    cleanup (emitter, _) = do
        c_yaml_emitter_delete emitter
        free emitter

    go (emitter, a) =
        loop
      where
#if MIN_VERSION_conduit(1, 0, 0)
        loop = await >>= maybe (close ()) push
#else
        loop = awaitE >>= either close push
#endif

        push e = do
            _ <- liftIO $ toEventRaw e $ c_yaml_emitter_emit emitter
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
