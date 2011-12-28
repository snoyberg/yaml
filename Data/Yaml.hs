{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-|
As a bit of background, this package is built on a few other packages I wrote.
yaml is a low-level wrapper around the C libyaml library, with an enumerator
interface. data-object is a package defining a data type:

@
    data Object k v = Scalar v
                    | Sequence [Object k v]
                    | Mapping [(k, Object k v)]
@

In other words, it can represent JSON data fully, and YAML data almost fully.
In particular, it doesn't handle cyclical aliases, which I hope doesn't really
occur too much in real life.

Another package to deal with is failure: it basically replaces using an Either
for error-handling into a typeclass. It has instances for Maybe, IO and lists
    by default.

The last package is convertible-text, which is a fork of John Goerzen's
convertible package. The difference is it supports both conversions that are
guaranteed to succeed (Int -> String) and ones which may fail (String -> Int),
and also supports various textual datatypes (String, lazy\/strict ByteString,
lazy\/string Text).

/YamlScalar and YamlObject/

We have a @type YamlObject = Object YamlScalar YamlScalar@, where a YamlScalar
is just a ByteString value with a tag and a style. A \"style\" is how the data
was represented in the underlying YAML file: single quoted, double quoted, etc.

Then there is an IsYamlScalar typeclass, which provides fromYamlScalar and
toYamlScalar conversion functions. There are instances for all the
\"text-like\" datatypes: String, ByteString and Text. The built-in instances
all assume a UTF-8 data encoding. And around this we have toYamlObject and
fromYamlObject functions, which do exactly what they sound like.

/Encoding and decoding/

There are two encoding files: encode and encodeFile. You can guess the
different: the former produces a ByteString (strict) and the latter writes to a
file. They both take an Object, whose keys and values must be an instance of
IsYamlScalar. So, for example:

@
    encodeFile "myfile.yaml" $ Mapping
        [ ("Michael", Mapping
            [ ("age", Scalar "26")
            , ("color", Scalar "blue")
            ])
        , ("Eliezer", Mapping
            [ ("age", Scalar "2")
            , ("color", Scalar "green")
            ])
        ]
@

decoding is only slightly more complicated, since the decoding can fail. In
particular, the return type is an IO wrapped around a Failure. For example, you
could use:

@
    maybeObject <- decodeFile "myfile.yaml"
    case maybeObject of
        Nothing -> putStrLn "Error parsing YAML file."
        Just object -> putStrLn "Successfully parsed."
@

If you just want to throw any parse errors as IO exception, you can use join:

@
    import Control.Monad (join)
    object <- join $ decodeFile "myfile.yaml"
@

This takes advantage of the IO instance of Failure.

/Parsing an Object/

In order to pull the data out of an Object, you can use the helper functions
from Data.Object. For example:

@
    import Data.Object
    import Data.Object.Yaml
    import Control.Monad

    main = do
        object <- join $ decodeFile "myfile.yaml"
        people <- fromMapping object
        michael <- lookupMapping "Michael" people
        age <- lookupScalar "age" michael
        putStrLn $ "Michael is " ++ age ++ " years old."
@

lookupScalar and friends implement Maybe, so you can test for optional 
attributes by switching on Nothing/Just a:

@
    name <- lookupScalar "middleName" michael :: Maybe String
@


/And that's it/

There's really not more to know about this library. Enjoy!
-}
module Data.Yaml
    ( -- * Definition of 'YamlObject'
      YamlScalar (..)
    , YamlObject
      -- * Automatic scalar conversions
    , IsYamlScalar (..)
    , toYamlObject
    , fromYamlObject
      -- * Encoding/decoding
    , encode
    , encodeFile
    , decode
    , decodeFile
      -- * Exceptions
    , ParseException (..)
    ) where

import qualified Text.Libyaml as Y
import Text.Libyaml hiding (encode, decode, encodeFile, decodeFile)
import Data.Object
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Control.Failure (Failure (failure))

import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

import Data.Convertible.Text (cs)
import Data.Data

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Prelude hiding (catch)
import Control.Exception (throwIO, Exception, fromException, try)
import Data.String (IsString (fromString))

-- | Equality depends on 'value' and 'tag', not 'style'.
data YamlScalar = YamlScalar
    { value :: ByteString
    , tag :: Tag
    , style :: Style
    }
    deriving (Show, Read, Data, Typeable)
instance Eq YamlScalar where
    (YamlScalar v t _) == (YamlScalar v' t' _) = v == v' && t == t'
instance IsString YamlScalar where
    fromString = toYamlScalar

type YamlObject = Object YamlScalar YamlScalar

class (Eq a) => IsYamlScalar a where
    fromYamlScalar :: YamlScalar -> a
    toYamlScalar :: a -> YamlScalar
instance IsYamlScalar YamlScalar where
    fromYamlScalar = id
    toYamlScalar = id
instance IsYamlScalar Data.Text.Text where
    fromYamlScalar = cs . value
    toYamlScalar t = YamlScalar (cs t) NoTag Any
instance IsYamlScalar Data.Text.Lazy.Text where
    fromYamlScalar = cs . value
    toYamlScalar t = YamlScalar (cs t) NoTag Any
instance IsYamlScalar [Char] where
    fromYamlScalar = cs . value
    toYamlScalar s = YamlScalar (cs s) NoTag Any
instance IsYamlScalar Data.ByteString.ByteString where
    fromYamlScalar = value
    toYamlScalar b = YamlScalar b NoTag Any
instance IsYamlScalar Data.ByteString.Lazy.ByteString where
    fromYamlScalar = cs . value
    toYamlScalar b = YamlScalar (cs b) NoTag Any

-- | Merge assoc-lists by keys.
-- First list overrides second:
--     [(k1, x), (k2, y)] `mergeAssocLists` [(k3, z)] == [(k1, x), (k2, y), (k3, z)]
--     [(k1, x), (k2, y)] `mergeAssocLists` [(k2, z)] == [(k1, x), (k2, y)]
mergeAssocLists :: (Eq k) => [(k, v)] -> [(k, v)] -> [(k, v)]
mergeAssocLists a [] = a
mergeAssocLists [] b = b
mergeAssocLists a ((bk, bv):bs) =
    case lookup bk a of
      Nothing -> (bk, bv) : mergeAssocLists a bs
      Just av -> (bk, av) : mergeAssocLists (filter (\(x, _) -> x /= bk) a) bs

toYamlObject :: IsYamlScalar k
             => IsYamlScalar v
             => Object k v
             -> YamlObject
toYamlObject = mapKeysValues toYamlScalar toYamlScalar

fromYamlObject :: IsYamlScalar k
               => IsYamlScalar v
               => YamlObject
               -> Object k v
fromYamlObject = mapKeysValues fromYamlScalar fromYamlScalar

encode :: (IsYamlScalar k, IsYamlScalar v) => Object k v -> ByteString
encode obj = unsafePerformIO $
    C.runResourceT $ CL.sourceList (objToEvents $ toYamlObject obj)
                C.$$ Y.encode

encodeFile :: (IsYamlScalar k, IsYamlScalar v)
           => FilePath
           -> Object k v
           -> IO ()
encodeFile fp obj = C.runResourceT
            $ CL.sourceList (objToEvents $ toYamlObject obj)
         C.$$ Y.encodeFile fp

objToEvents :: YamlObject -> [Y.Event]
objToEvents o = (:) EventStreamStart
              . (:) EventDocumentStart
              $ objToEvents' o
              [ EventDocumentEnd
              , EventStreamEnd
              ]

scalarToEvent :: YamlScalar -> Event
scalarToEvent (YamlScalar v t s) = EventScalar v t s Nothing

objToEvents' :: YamlObject -> [Y.Event] -> [Y.Event]
objToEvents' (Scalar s) rest = scalarToEvent s : rest
objToEvents' (Sequence list) rest =
    EventSequenceStart Nothing
  : foldr ($) (EventSequenceEnd : rest) (map objToEvents' list)
objToEvents' (Mapping pairs) rest =
    EventMappingStart Nothing
  : foldr ($) (EventMappingEnd : rest) (map pairToEvents pairs)

pairToEvents :: (YamlScalar, YamlObject) -> [Y.Event] -> [Y.Event]
pairToEvents (k, v) rest =
    scalarToEvent k
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

type Parser = StateT (Map.Map String YamlObject) IO

requireEvent :: Event -> C.Sink Event Parser ()
requireEvent e = do
    f <- CL.head
    if f == Just e
        then return ()
        else liftIO $ throwIO $ UnexpectedEvent f $ Just e

parse :: C.Sink Event Parser YamlObject
parse = do
    requireEvent EventStreamStart
    requireEvent EventDocumentStart
    res <- parseO
    requireEvent EventDocumentEnd
    requireEvent EventStreamEnd
    return res

parseScalar :: ByteString -> Tag -> Style -> Anchor
            -> C.Sink Event Parser YamlScalar
parseScalar v t s a = do
    let res = YamlScalar v t s
    case a of
        Nothing -> return res
        Just an -> do
            lift $ modify (Map.insert an $ Scalar res)
            return res

parseO :: C.Sink Event Parser YamlObject
parseO = do
    me <- CL.head
    case me of
        Just (EventScalar v t s a) -> Scalar `liftM` parseScalar v t s a
        Just (EventSequenceStart a) -> parseS a id
        Just (EventMappingStart a) -> parseM a id
        Just (EventAlias an) -> do
            m <- lift get
            case Map.lookup an m of
                Nothing -> liftIO $ throwIO $ UnknownAlias an
                Just v -> return v
        _ -> liftIO $ throwIO $ UnexpectedEvent me Nothing

parseS :: Y.Anchor
       -> ([YamlObject] -> [YamlObject])
       -> C.Sink Event Parser YamlObject
parseS a front = do
    me <- CL.peek
    case me of
        Just EventSequenceEnd -> do
            CL.drop 1
            let res = Sequence $ front []
            case a of
                Nothing -> return res
                Just an -> do
                    lift $ modify $ Map.insert an res
                    return res
        _ -> do
            o <- parseO
            parseS a $ front . (:) o

parseM :: Y.Anchor
       -> ([(YamlScalar, YamlObject)] -> [(YamlScalar, YamlObject)])
       -> C.Sink Event Parser YamlObject
parseM a front = do
    me <- CL.peek
    case me of
        Just EventMappingEnd -> do
            CL.drop 1
            let res = Mapping $ front []
            case a of
                Nothing -> return res
                Just an -> do
                    lift $ modify $ Map.insert an res
                    return res
        _ -> do
            me' <- CL.head
            s <- case me' of
                    Just (EventScalar v t s a') -> parseScalar v t s a'
                    _ -> liftIO $ throwIO $ UnexpectedEvent me' Nothing
            o <- parseO
            let al  = mergeAssocLists [(s, o)] $ front []
                al' = if fromYamlScalar s == "<<"
                         then case o of
                                  Scalar _    -> al
                                  Mapping l  -> mergeAssocLists al l
                                  Sequence l -> mergeAssocLists al $ foldl merge' [] l
                         else al
            parseM a (`mergeAssocLists` al')
    where merge' :: (Eq k) => [(k, Object k v)] -> Object k v -> [(k, Object k v)]
          merge' al (Mapping om) = mergeAssocLists al om
          merge' al _            = al

decode :: (Failure ParseException m, IsYamlScalar k, IsYamlScalar v)
       => ByteString
       -> m (Object k v)
decode bs = unsafePerformIO $ decodeHelper (Y.decode bs)

decodeFile :: (Failure ParseException m, IsYamlScalar k, IsYamlScalar v)
           => FilePath
           -> IO (m (Object k v))
decodeFile fp = decodeHelper (Y.decodeFile fp)

decodeHelper :: (Failure ParseException m, IsYamlScalar k, IsYamlScalar v)
             => C.Source Parser Y.Event
             -> IO (m (Object k v))
decodeHelper src = do
    x <- try $ flip evalStateT Map.empty $ C.runResourceT $ src C.$$ parse
    case x of
        Left e
            | Just pe <- fromException e -> return $ failure (pe :: ParseException)
            | otherwise -> return $ failure $ InvalidYaml $ Just $ show e
        Right y -> return $ return $ fromYamlObject y
