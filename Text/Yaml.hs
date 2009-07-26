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

import Text.Libyaml
import qualified Data.ByteString as B
import Data.ByteString.Class
import Data.Object
import System.IO.Unsafe

encodeFile :: IsObject o => FilePath -> o -> IO ()
encodeFile path o = B.writeFile path $ encode o

decodeFile :: (Monad m, IsObject o) => FilePath -> IO (m o)
decodeFile path = decode `fmap` B.readFile path

encode :: (StrictByteString bs, IsObject o) => o -> bs
encode o =
    let events = objectToEvents $ toObject o
        result = withEmitter $ flip emitEvents $ events
     in fromStrictByteString $ unsafePerformIO result

decode :: (Monad m, StrictByteString bs, IsObject o) => bs -> m o
decode bs = do
    let eventsIO = withParser $ \p -> do
                        parserSetInputString p $ toStrictByteString bs
                        parserParse p
        events = unsafePerformIO eventsIO
    fromObject $ eventsToObject events

objectToEvents :: Object -> [Event]
objectToEvents y = concat
                    [ [EventStreamStart, EventDocumentStart]
                    , writeSingle y
                    , [EventDocumentEnd, EventStreamEnd]] where
    writeSingle :: Object -> [Event]
    writeSingle (Scalar bs) = [EventScalar bs]
    writeSingle (Sequence ys) =
        (EventSequenceStart : concatMap writeSingle ys)
        ++ [EventSequenceEnd]
    writeSingle (Mapping pairs) =
        EventMappingStart : concatMap writePair pairs ++ [EventMappingEnd]
    writePair :: (B.ByteString, Object) -> [Event]
    writePair (k, v) = EventScalar k : writeSingle v

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
    readSingle (EventScalar bs:rest) = (Scalar bs, rest)
    readSingle (EventSequenceStart:rest) = readList [] rest
    readSingle (EventMappingStart:rest) = readMap [] rest
    readSingle (x:rest) = error $ "readSingle: " ++ show x
    readList :: [Object] -> [Event] -> (Object, [Event])
    readList nodes (EventSequenceEnd:rest) =
        (Sequence $ reverse nodes, rest)
    readList nodes events =
        let (next, rest) = readSingle events
         in readList (next : nodes) rest
    readMap :: [(B.ByteString, Object)] -> [Event] -> (Object, [Event])
    readMap pairs (EventMappingEnd:rest) = (Mapping $ reverse pairs, rest)
    readMap pairs (EventScalar bs:events) =
        let (next, rest) = readSingle events
         in readMap ((fromStrictByteString bs, next) : pairs) rest
