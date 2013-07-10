{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provides a high-level interface for processing YAML files.
--
-- This module reuses most of the infrastructure from the @aeson@ package.
-- This means that you can use all of the existing tools for JSON
-- processing for processing YAML files. As a result, much of the
-- documentation below mentions JSON; do not let that confuse you, it's
-- intentional.
--
-- For the most part, YAML content translates directly into JSON, and
-- therefore there is very little data loss. If you need to deal with YAML
-- more directly (e.g., directly deal with aliases), you should use the
-- "Text.Libyaml" module instead.
--
-- For documentation on the @aeson@ types, functions, classes, and
-- operators, please see the @Data.Aeson@ module of the @aeson@ package.
module Data.Yaml
    ( -- * Types
      Value (..)
    , Parser
    , Object
    , Array
    , ParseException(..)
    , YamlException (..)
    , YamlMark (..)
      -- * Constructors and accessors
    , object
    , array
    , (.=)
    , (.:)
    , (.:?)
    , (.!=)
      -- * Parsing
    , parseMonad
    , parseEither
    , parseMaybe
      -- * Classes
    , ToJSON (..)
    , FromJSON (..)
      -- * Encoding/decoding
    , encode
    , encodeFile
    , decode
    , decodeFile
      -- ** Better error information
    , decodeEither
    , decodeEither'
    , decodeFileEither
      -- ** More control over decoding
    , decodeHelper
    ) where

import qualified Text.Libyaml as Y
import Data.Aeson
    ( Value (..), ToJSON (..), FromJSON (..), object
    , (.=) , (.:) , (.:?) , (.!=)
    , Object, Array
    )
import Data.Aeson.Types (Pair, parseMaybe, parseEither, Parser)
import Text.Libyaml hiding (encode, decode, encodeFile, decodeFile)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (try, throwIO, fromException, Exception, SomeException, AsyncException)
import Control.Monad.Trans.State
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (liftM)
import Data.Char (toUpper)
import qualified Data.Vector as V
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Read (signed, decimal, double)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.HashMap.Strict as M
import Data.Typeable
import Data.Attoparsec.Number

encode :: ToJSON a => a -> ByteString
encode obj = unsafePerformIO $
    C.runResourceT $ CL.sourceList (objToEvents $ toJSON obj)
                C.$$ Y.encode

encodeFile :: ToJSON a => FilePath -> a -> IO ()
encodeFile fp obj = C.runResourceT
            $ CL.sourceList (objToEvents $ toJSON obj)
         C.$$ Y.encodeFile fp

objToEvents :: Value -> [Y.Event]
objToEvents o = (:) EventStreamStart
              . (:) EventDocumentStart
              $ objToEvents' o
              [ EventDocumentEnd
              , EventStreamEnd
              ]

{- FIXME
scalarToEvent :: YamlScalar -> Event
scalarToEvent (YamlScalar v t s) = EventScalar v t s Nothing
-}

objToEvents' :: Value -> [Y.Event] -> [Y.Event]
--objToEvents' (Scalar s) rest = scalarToEvent s : rest
objToEvents' (Array list) rest =
    EventSequenceStart Nothing
  : foldr ($) (EventSequenceEnd : rest) (map objToEvents' $ V.toList list)
objToEvents' (Object pairs) rest =
    EventMappingStart Nothing
  : foldr ($) (EventMappingEnd : rest) (map pairToEvents $ M.toList pairs)

-- Empty strings need special handling to ensure they get quoted. This avoids:
-- https://github.com/snoyberg/yaml/issues/24
objToEvents' (String "") rest = EventScalar "" NoTag SingleQuoted Nothing : rest

objToEvents' (String s) rest = EventScalar (encodeUtf8 s) StrTag PlainNoTag Nothing : rest
objToEvents' Null rest = EventScalar "null" NullTag PlainNoTag Nothing : rest
objToEvents' (Bool True) rest = EventScalar "true" BoolTag PlainNoTag Nothing : rest
objToEvents' (Bool False) rest = EventScalar "false" BoolTag PlainNoTag Nothing : rest
objToEvents' (Number n) rest = EventScalar (S8.pack $ show n) IntTag PlainNoTag Nothing : rest

pairToEvents :: Pair -> [Y.Event] -> [Y.Event]
pairToEvents (k, v) rest =
    EventScalar (encodeUtf8 k) StrTag PlainNoTag Nothing
  : objToEvents' v rest

-- Parsing

data ParseException = NonScalarKey
                    | UnknownAlias { _anchorName :: Y.AnchorName }
                    | UnexpectedEvent { _received :: Maybe Event
                                      , _expected :: Maybe Event
                                      }
                    | InvalidYaml (Maybe YamlException)
                    | AesonException String
                    | OtherParseException SomeException
    deriving (Show, Typeable)
instance Exception ParseException

newtype PErrorT m a = PErrorT { runPErrorT :: m (Either ParseException a) }
instance Monad m => Monad (PErrorT m) where
    return = PErrorT . return . Right
    (PErrorT m) >>= f = PErrorT $ do
        e <- m
        case e of
            Left e' -> return $ Left e'
            Right a -> runPErrorT $ f a
instance MonadTrans PErrorT where
    lift = PErrorT . liftM Right
instance MonadIO m => MonadIO (PErrorT m) where
    liftIO = lift . liftIO

type Parse = StateT (Map.Map String Value) (C.ResourceT IO)

requireEvent :: Event -> C.Sink Event Parse ()
requireEvent e = do
    f <- CL.head
    if f == Just e
        then return ()
        else liftIO $ throwIO $ UnexpectedEvent f $ Just e

parse :: C.Sink Event Parse Value
parse = do
    requireEvent EventStreamStart
    requireEvent EventDocumentStart
    res <- parseO
    requireEvent EventDocumentEnd
    requireEvent EventStreamEnd
    return res

parseScalar :: ByteString -> Anchor -> Style -> Tag
            -> C.Sink Event Parse Text
parseScalar v a style tag = do
    let res = decodeUtf8With lenientDecode v
    case a of
        Nothing -> return res
        Just an -> do
            lift $ modify (Map.insert an $ textToValue style tag res)
            return res

textToValue :: Style -> Tag -> Text -> Value
textToValue SingleQuoted _ t = String t
textToValue DoubleQuoted _ t = String t
textToValue _ StrTag t = String t
textToValue Folded _ t = String t
textToValue _ _ t
    | t `elem` ["null", "Null", "NULL", "~", ""] = Null
    | any (t `isLike`) ["y", "yes", "on", "true"] = Bool True
    | any (t `isLike`) ["n", "no", "off", "false"] = Bool False
    | Right (x, "") <- signed decimal t = Number $ I x
    | Right (x, "") <- double t = Number $ D x
    | otherwise = String t
  where x `isLike` ref = x `elem` [ref, T.toUpper ref, titleCased]
          where titleCased = toUpper (T.head ref) `T.cons` T.tail ref


parseO :: C.Sink Event Parse Value
parseO = do
    me <- CL.head
    case me of
        Just (EventScalar v tag style a) -> fmap (textToValue style tag) $ parseScalar v a style tag
        Just (EventSequenceStart a) -> parseS a id
        Just (EventMappingStart a) -> parseM a M.empty
        Just (EventAlias an) -> do
            m <- lift get
            case Map.lookup an m of
                Nothing -> liftIO $ throwIO $ UnknownAlias an
                Just v -> return v
        _ -> liftIO $ throwIO $ UnexpectedEvent me Nothing

parseS :: Y.Anchor
       -> ([Value] -> [Value])
       -> C.Sink Event Parse Value
parseS a front = do
    me <- CL.peek
    case me of
        Just EventSequenceEnd -> do
            CL.drop 1
            let res = Array $ V.fromList $ front []
            case a of
                Nothing -> return res
                Just an -> do
                    lift $ modify $ Map.insert an res
                    return res
        _ -> do
            o <- parseO
            parseS a $ front . (:) o

parseM :: Y.Anchor
       -> M.HashMap Text Value
       -> C.Sink Event Parse Value
parseM a front = do
    me <- CL.peek
    case me of
        Just EventMappingEnd -> do
            CL.drop 1
            let res = Object front
            case a of
                Nothing -> return res
                Just an -> do
                    lift $ modify $ Map.insert an res
                    return res
        _ -> do
            CL.drop 1
            s <- case me of
                    Just (EventScalar v tag style a') -> parseScalar v a' style tag
                    _ -> liftIO $ throwIO $ UnexpectedEvent me Nothing
            o <- parseO

            let al  = M.insert s o front
                al' = if s == pack "<<"
                         then case o of
                                  Object l  -> M.union al l
                                  Array l -> M.union al $ foldl merge' M.empty $ V.toList l
                                  _          -> al
                         else al
            parseM a $ M.insert s o al'
    where merge' al (Object om) = M.union al om
          merge' al _           = al

decode :: FromJSON a
       => ByteString
       -> Maybe a
decode bs = unsafePerformIO
          $ fmap (either (const Nothing) (either (const Nothing) Just))
          $ decodeHelper (Y.decode bs)

decodeFile :: FromJSON a
           => FilePath
           -> IO (Maybe a)
decodeFile fp = decodeHelper (Y.decodeFile fp) >>= either throwIO (return . either (const Nothing) id)

-- | A version of 'decodeFile' which should not throw runtime exceptions.
--
-- Since 0.8.4
decodeFileEither
    :: FromJSON a
    => FilePath
    -> IO (Either ParseException a)
decodeFileEither fp = do
    x <- try $ C.runResourceT $ flip evalStateT Map.empty $ Y.decodeFile fp C.$$ parse
    case x of
        Left e
            | Just pe <- fromException e -> return $ Left pe
            | Just ye <- fromException e -> return $ Left $ InvalidYaml $ Just (ye :: YamlException)
            | shouldBeCaught e -> return $ Left $ OtherParseException e
            | otherwise -> throwIO e
        Right y ->
            return $ case parseEither parseJSON y of
                Left s -> Left $ AesonException s
                Right v -> Right v
  where
    -- FIXME this function needs more thought
    shouldBeCaught e
        | Just (_ :: AsyncException) <- fromException e = False
        -- Would be nice... | Just (_ :: Timeout) <- fromException e = False
        | otherwise = True

decodeEither :: FromJSON a => ByteString -> Either String a
decodeEither bs = unsafePerformIO
                $ fmap (either (Left . show) id)
                $ decodeHelper (Y.decode bs)

-- | More helpful version of 'decodeEither' which returns the 'YamlException'.
--
-- Since 0.8.3
decodeEither' :: FromJSON a => ByteString -> Either ParseException a
decodeEither' = either Left (either (Left . AesonException) Right)
              . unsafePerformIO
              . decodeHelper
              . Y.decode

decodeHelper :: FromJSON a
             => C.Source Parse Y.Event
             -> IO (Either ParseException (Either String a))
decodeHelper src = do
    x <- try $ C.runResourceT $ flip evalStateT Map.empty $ src C.$$ parse
    case x of
        Left e
            | Just pe <- fromException e -> return $ Left pe
            | Just ye <- fromException e -> return $ Left $ InvalidYaml $ Just (ye :: YamlException)
            | otherwise -> throwIO e
        Right y -> return $ Right $ parseEither parseJSON y

array :: [Value] -> Value
array = Array . V.fromList

parseMonad :: Monad m => (a -> Parser b) -> a -> m b
parseMonad p = either fail return . parseEither p
