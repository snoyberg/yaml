{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Yaml
    ( -- * Types
      Value (..)
    , object
    , array
      -- * Classes
    , ToJSON (..)
    , FromJSON (..)
      -- * Encoding/decoding
    , encode
    , encodeFile
    , decode
    , decodeFile
    ) where

import qualified Text.Libyaml as Y
import Data.Aeson (Value (..), ToJSON (..), FromJSON (..), object)
import Data.Aeson.Types (Pair, parseMaybe)
import Text.Libyaml hiding (encode, decode, encodeFile, decodeFile)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (try, throwIO, fromException, Exception)
import Control.Monad.Trans.State
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (liftM)
import qualified Data.Vector as V
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.HashMap.Strict as M
import Data.Typeable

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
objToEvents' (String s) rest = EventScalar (encodeUtf8 s) StrTag Any Nothing : rest

pairToEvents :: Pair -> [Y.Event] -> [Y.Event]
pairToEvents (k, v) rest =
    EventScalar (encodeUtf8 k) StrTag Any Nothing
  : objToEvents' v rest

-- Parsing

data ParseException = NonScalarKey
                    | UnknownAlias { _anchorName :: Y.AnchorName }
                    | UnexpectedEvent { _received :: Maybe Event
                                      , _expected :: Maybe Event
                                      }
                    | InvalidYaml (Maybe String)
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

type Parser = StateT (Map.Map String Value) IO

requireEvent :: Event -> C.Sink Event Parser ()
requireEvent e = do
    f <- CL.head
    if f == Just e
        then return ()
        else liftIO $ throwIO $ UnexpectedEvent f $ Just e

parse :: C.Sink Event Parser Value
parse = do
    requireEvent EventStreamStart
    requireEvent EventDocumentStart
    res <- parseO
    requireEvent EventDocumentEnd
    requireEvent EventStreamEnd
    return res

parseScalar :: ByteString -> Anchor
            -> C.Sink Event Parser Text
parseScalar v a = do
    let res = decodeUtf8With lenientDecode v
    case a of
        Nothing -> return res
        Just an -> do
            lift $ modify (Map.insert an $ String res)
            return res

parseO :: C.Sink Event Parser Value
parseO = do
    me <- CL.head
    case me of
        Just (EventScalar v _t _s a) -> fmap String $ parseScalar v a
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
       -> C.Sink Event Parser Value
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
       -> C.Sink Event Parser Value
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
                    Just (EventScalar v _ _ a') -> parseScalar v a'
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
decode bs = unsafePerformIO $ fmap (either (const Nothing) id) $ decodeHelper (Y.decode bs)

decodeFile :: FromJSON a
           => FilePath
           -> IO (Maybe a)
decodeFile fp = decodeHelper (Y.decodeFile fp) >>= either throwIO return

decodeHelper :: FromJSON a
             => C.Source Parser Y.Event
             -> IO (Either ParseException (Maybe a))
decodeHelper src = do
    x <- try $ flip evalStateT Map.empty $ C.runResourceT $ src C.$$ parse
    case x of
        Left e
            | Just pe <- fromException e -> return $ Left pe
            | Just ye <- fromException e -> return $ Left $ InvalidYaml $ Just $ show (ye :: YamlException)
            | otherwise -> throwIO e
        Right y -> return $ Right $ parseMaybe parseJSON y

array :: [Value] -> Value
array = Array . V.fromList
