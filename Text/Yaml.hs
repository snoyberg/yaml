{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    , testSuite
    ) where

import Prelude hiding (readList)
import Text.Libyaml
import qualified Data.ByteString as B
import Data.ByteString.Class
import Data.Object
import Data.Object.Raw
import System.IO.Unsafe
import Control.Arrow (first, (***))
import Control.Monad (replicateM, liftM)
import Control.Monad.Trans
import Control.Monad.Attempt

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit hiding (Test, path)
import Test.QuickCheck

import Control.Exception

encode :: (StrictByteString bs, ToObject o Raw Raw) => o -> bs
encode = unsafePerformIO . encode'

decode :: (StrictByteString bs, FromObject o Raw Raw, MonadFailure SomeException m)
       => bs
       -> m o
decode bs = unsafePerformIO $ do
    a <- runAttemptT $ decode' bs -- FIXME
    case a of
        Success s -> return $ return s
        Failure e -> return $ failure e

encodeFile :: ToObject o Raw Raw => FilePath -> o -> IO ()
encodeFile path o = encode' o >>= B.writeFile path

decodeFile :: (FromObject o Raw Raw, MonadFailure SomeException m, MonadIO m)
           => FilePath
           -> m o
decodeFile path = liftIO (B.readFile path) >>= decode'

encode' :: (StrictByteString bs, ToObject o Raw Raw) => o -> IO bs
encode' o =
    let events = objectToEvents $ toObject o
        result = withEmitter $ flip emitEvents events
     in fromStrictByteString `fmap` joinAttempt result

decode' :: (StrictByteString bs, FromObject o Raw Raw, MonadIO m,
            MonadFailure SomeException m)
        => bs
        -> m o
decode' bs = do
    events <- liftIO $ withParser (toStrictByteString bs) parserParse
    attempt failure' (myFA . fromObject . eventsToRawObject) events
    where
        myFA = attempt failure' return
        failure' :: (MonadFailure SomeException m, Exception e) => e -> m a
        failure' = failure . SomeException

objectToEvents :: RawObject -> [Event]
objectToEvents y = concat
                    [ [EventStreamStart, EventDocumentStart]
                    , writeSingle y
                    , [EventDocumentEnd, EventStreamEnd]] where
    writeSingle :: RawObject -> [Event]
    writeSingle (Scalar (Raw bs)) = [EventScalar $ toStrictByteString bs]
    writeSingle (Sequence ys) =
        (EventSequenceStart : concatMap writeSingle ys)
        ++ [EventSequenceEnd]
    writeSingle (Mapping pairs) =
        EventMappingStart : concatMap writePair pairs ++ [EventMappingEnd]
    writePair :: (Raw, RawObject) -> [Event]
    writePair (Raw k, v) = EventScalar (fromLazyByteString k) : writeSingle v

eventsToRawObject :: [Event] -> RawObject
eventsToRawObject = fst . readSingle . dropWhile isIgnored where
    isIgnored EventAlias = False
    isIgnored (EventScalar _) = False
    isIgnored EventSequenceStart = False
    isIgnored EventSequenceEnd = False
    isIgnored EventMappingStart = False
    isIgnored EventMappingEnd = False
    isIgnored _ = True
    readSingle :: [Event] -> (RawObject, [Event])
    readSingle [] = error "readSingle: no more events"
    readSingle (EventScalar bs:rest) =
        (Scalar $ Raw $ toLazyByteString bs, rest)
    readSingle (EventSequenceStart:rest) = readList [] rest
    readSingle (EventMappingStart:rest) = readMap [] rest
    readSingle (x:_) = error $ "readSingle: " ++ show x
    readList :: [RawObject] -> [Event] -> (RawObject, [Event])
    readList nodes (EventSequenceEnd:rest) =
        (Sequence $ reverse nodes, rest)
    readList nodes events =
        let (next, rest) = readSingle events
         in readList (next : nodes) rest
    readMap :: [(B.ByteString, RawObject)]
            -> [Event]
            -> (RawObject, [Event])
    readMap pairs (EventMappingEnd:rest) =
        (Mapping $ map (first $ Raw . toLazyByteString) $ reverse pairs, rest)
    readMap pairs (EventScalar bs:events) =
        let (next, rest) = readSingle events
         in readMap ((fromStrictByteString bs, next) : pairs) rest
    readMap _ (e:_) = error $ "Unexpected event in readMap: " ++ show e
    readMap _ [] = error "Unexpected empty event list in readMap"

newtype MyString = MyString String
    deriving (Eq, Show)
instance ToScalar MyString Raw where
    toScalar (MyString s) = toScalar s
instance FromScalar MyString Raw where
    fromScalar raw = MyString `liftM` fromScalar raw

propEncodeDecode :: Object MyString MyString -> Bool
propEncodeDecode o = fromAttempt (decode (encode o :: B.ByteString))
                     == Just o

caseEmptyStrings :: Assertion
caseEmptyStrings = do
    let m =
            [ ("foo", "bar")
            , ("baz", "")
            , ("bin", "")
            ]
    let m' = map (toStrictByteString *** toStrictByteString) m
    let m'' = map (toLazyByteString *** toLazyByteString) m
    let test' x = Just x @=? fromAttempt (decode (encode x :: String))
    test' m
    test' m'
    test' m''

testSuite :: Test
testSuite = testGroup "Text.Yaml"
    [ testProperty "propEncodeDecode" propEncodeDecode
    , testCase "empty strings" caseEmptyStrings
    ]

instance Arbitrary (Object MyString MyString) where
    coarbitrary = undefined
    arbitrary = oneof [arbS, arbL, arbM] where
        arbS = Scalar `fmap` (arbitrary :: Gen MyString)
        arbL = Sequence `fmap` vector 1
        arbM = Mapping `fmap` vector 1

instance Arbitrary MyString where
    coarbitrary = undefined
    arbitrary = do
        size <- arbitrary
        s <- replicateM (size `mod` 5) $ elements ['A'..'Z']
        return $! MyString s
