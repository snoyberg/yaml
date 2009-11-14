{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
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
#if TEST
    , testSuite
#endif
    ) where

import Prelude hiding (readList)
import Text.Libyaml
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Object
import Data.Object.Text
import System.IO.Unsafe
import Control.Arrow (first)
import Control.Monad (liftM, join)
import Control.Monad.Trans
import Control.Monad.Attempt
import Data.Convertible

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit hiding (Test, path)
import Test.QuickCheck
#endif

encode :: (ConvertSuccess B.ByteString a, ToObject o Text Text) => o -> a
encode = unsafePerformIO . encode'

decode :: (ConvertSuccess a B.ByteString,
           MonadFailure YamlException m)
       => a
       -> m TextObject
decode = unsafePerformIO . decode'

encodeFile :: ToObject o Text Text => FilePath -> o -> IO ()
encodeFile path o = encode' o >>= ltWriteFile path

ltWriteFile :: FilePath -> Text -> IO ()
ltWriteFile fp = BL.writeFile fp . LTE.encodeUtf8

decodeFile :: (MonadFailure YamlException m,
               MonadIO m)
           => FilePath
           -> m TextObject
decodeFile path = do
    contents <- liftIO (ltReadFile path)
    join $ liftIO $ decode' contents

ltReadFile :: FilePath -> IO Text
ltReadFile = fmap LTE.decodeUtf8 . BL.readFile

encode' :: (ConvertSuccess B.ByteString a,
            ToObject o Text Text)
        => o
        -> IO a
encode' o =
    let events = objectToEvents $ toObject o
        result = withEmitter $ flip emitEvents events
     in convertSuccess `fmap` joinAttempt result

decode' :: (ConvertSuccess a B.ByteString,
            MonadFailure YamlException m
           )
        => a
        -> IO (m TextObject)
decode' bs = do
    events <- withParser (convertSuccess bs) parserParse
    return $ liftM eventsToTextObject events

objectToEvents :: TextObject -> [Event]
objectToEvents y = concat
                    [ [EventStreamStart, EventDocumentStart]
                    , writeSingle y
                    , [EventDocumentEnd, EventStreamEnd]] where
    writeSingle :: TextObject -> [Event]
    writeSingle (Scalar text) = [EventScalar $ convertSuccess text]
    writeSingle (Sequence ys) =
        (EventSequenceStart : concatMap writeSingle ys)
        ++ [EventSequenceEnd]
    writeSingle (Mapping pairs) =
        EventMappingStart : concatMap writePair pairs ++ [EventMappingEnd]
    writePair :: (Text, TextObject) -> [Event]
    writePair (k, v) = EventScalar (convertSuccess k) : writeSingle v

eventsToTextObject :: [Event] -> TextObject
eventsToTextObject = fst . readSingle . dropWhile isIgnored where
    isIgnored EventAlias = False
    isIgnored (EventScalar _) = False
    isIgnored EventSequenceStart = False
    isIgnored EventSequenceEnd = False
    isIgnored EventMappingStart = False
    isIgnored EventMappingEnd = False
    isIgnored _ = True
    readSingle :: [Event] -> (TextObject, [Event])
    readSingle [] = error "readSingle: no more events"
    readSingle (EventScalar bs:rest) =
        (Scalar $ convertSuccess bs, rest)
    readSingle (EventSequenceStart:rest) = readList [] rest
    readSingle (EventMappingStart:rest) = readMap [] rest
    readSingle (x:_) = error $ "readSingle: " ++ show x
    readList :: [TextObject] -> [Event] -> (TextObject, [Event])
    readList nodes (EventSequenceEnd:rest) =
        (Sequence $ reverse nodes, rest)
    readList nodes events =
        let (next, rest) = readSingle events
         in readList (next : nodes) rest
    readMap :: [(B.ByteString, TextObject)]
            -> [Event]
            -> (TextObject, [Event])
    readMap pairs (EventMappingEnd:rest) =
        (Mapping $ map (first convertSuccess) $ reverse pairs, rest)
    readMap pairs (EventScalar bs:events) =
        let (next, rest) = readSingle events
         in readMap ((bs, next) : pairs) rest
    readMap _ (e:_) = error $ "Unexpected event in readMap: " ++ show e
    readMap _ [] = error "Unexpected empty event list in readMap"

newtype MyString = MyString String
    deriving (Eq, Show)
instance ConvertAttempt MyString Text where
    convertAttempt = return . convertSuccess
instance ConvertSuccess MyString Text where
    convertSuccess (MyString s) = convertSuccess s
instance ConvertAttempt Text MyString where
    convertAttempt raw = MyString `liftM` convertAttempt raw

#if TEST
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
#endif
