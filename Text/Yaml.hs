---------------------------------------------------------
--
-- Module        : Text.Yaml
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Provides a user friendly interface for serializing Yaml data.
--
---------------------------------------------------------

module Text.Yaml
    ( module Data.Object
    , encode
    , encodeFile
    , decode
    , decodeFile
    ) where

import Prelude hiding (readList)
import Text.Libyaml
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Class
import Data.Object
import System.IO.Unsafe
import Control.Arrow (first)

encode :: (StrictByteString bs, ToObject o) => o -> bs
encode = unsafePerformIO . encode'

decode :: (Monad m, StrictByteString bs, FromObject o) => bs -> m o
decode = unsafePerformIO . decode'

encodeFile :: ToObject o => FilePath -> o -> IO ()
encodeFile path o = encode' o >>= B.writeFile path

decodeFile :: (Monad m, FromObject o) => FilePath -> IO (m o)
decodeFile path = do
    c <- B.readFile path
    decode' c

encode' :: (StrictByteString bs, ToObject o) => o -> IO bs
encode' o =
    let events = objectToEvents $ toObject o
        result = withEmitter $ flip emitEvents $ events
     in fromStrictByteString `fmap` result

decode' :: (Monad m, StrictByteString bs, FromObject o) => bs -> IO (m o)
decode' bs = do
    let bs' = toStrictByteString bs
        events = withParser bs' parserParse
    (fromObject . eventsToObject) `fmap` events

objectToEvents :: Object -> [Event]
objectToEvents y = concat
                    [ [EventStreamStart, EventDocumentStart]
                    , writeSingle y
                    , [EventDocumentEnd, EventStreamEnd]] where
    writeSingle :: Object -> [Event]
    writeSingle (Scalar bs) = [EventScalar $ fromLazyByteString bs]
    writeSingle (Sequence ys) =
        (EventSequenceStart : concatMap writeSingle ys)
        ++ [EventSequenceEnd]
    writeSingle (Mapping pairs) =
        EventMappingStart : concatMap writePair pairs ++ [EventMappingEnd]
    writePair :: (BL.ByteString, Object) -> [Event]
    writePair (k, v) = EventScalar (fromLazyByteString k) : writeSingle v

eventsToObject :: [Event] -> Object
eventsToObject = fst . readSingle . dropWhile isIgnored where
    isIgnored EventAlias = False
    isIgnored (EventScalar _) = False
    isIgnored EventSequenceStart = False
    isIgnored EventSequenceEnd = False
    isIgnored EventMappingStart = False
    isIgnored EventMappingEnd = False
    isIgnored _ = True
    readSingle :: [Event] -> (Object, [Event])
    readSingle [] = error "readSingle: no more events"
    readSingle (EventScalar bs:rest) = (Scalar $ toLazyByteString bs, rest)
    readSingle (EventSequenceStart:rest) = readList [] rest
    readSingle (EventMappingStart:rest) = readMap [] rest
    readSingle (x:_) = error $ "readSingle: " ++ show x
    readList :: [Object] -> [Event] -> (Object, [Event])
    readList nodes (EventSequenceEnd:rest) =
        (Sequence $ reverse nodes, rest)
    readList nodes events =
        let (next, rest) = readSingle events
         in readList (next : nodes) rest
    readMap :: [(B.ByteString, Object)] -> [Event] -> (Object, [Event])
    readMap pairs (EventMappingEnd:rest) =
        (Mapping $ map (first toLazyByteString) $ reverse pairs, rest)
    readMap pairs (EventScalar bs:events) =
        let (next, rest) = readSingle events
         in readMap ((fromStrictByteString bs, next) : pairs) rest
    readMap _ (e:_) = error $ "Unexpected event in readMap: " ++ show e
    readMap _ [] = error "Unexpected empty event list in readMap"
