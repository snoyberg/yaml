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
    (
      -- * Exceptions
      YamlException (..)
    ,
      -- * Converting to/from 'TextObject's
      encodeText
    , encodeToText
    , decodeText
    , decodeFromText
    , decodeFromTextWrap
      -- * Convert to/from 'ScalarObejct's
    , encodeScalar
    , decodeScalar
      -- * Files
    , encodeFile
    , encodeFileToText
    , decodeFile
    , decodeFileFromTextWrap
#if TEST
    , testSuite
#endif
    ) where

import Prelude hiding (readList)
import Text.Libyaml
import qualified Data.ByteString as B
import Data.Object
import Data.Object.Text
import Data.Object.Scalar
import System.IO.Unsafe
import Control.Arrow (first)
import Control.Monad (liftM, join)
import Control.Monad.Trans
import Data.Convertible
import Data.Attempt

import Data.Text.Lazy (Text)

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit hiding (Test, path)
import Test.QuickCheck

import Control.Monad (replicateM)
import Control.Arrow ((***))
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as BL
#endif

-- Low level, non-exported functions
encode' :: MonadFailure YamlException m
        => TextObject
        -> IO (m B.ByteString)
encode' o = do
    let events = objectToEvents o
    result <- withEmitter $ flip emitEvents events
    return $ do
        result' <- result
        return result'

decode' :: MonadFailure YamlException m
        => B.ByteString
        -> IO (m TextObject)
decode' bs = do
    events <- withParser (convertSuccess bs) parserParse
    return $ liftM eventsToTextObject events

-- Text API
-- FIXME: this *should* be in a MonadFailure

-- FIXME: I think the To and From below are backwards. Maybe just say convert?
encodeText :: ConvertSuccess B.ByteString a
           => TextObject
           -> a
encodeText = convertSuccess . unsafePerformIO . join . encode'

encodeToText :: (ConvertSuccess B.ByteString a, ToObject x Text Text)
             => x
             -> a
encodeToText = convertSuccess . unsafePerformIO . join . encode' . toTextObject

decodeText :: (ConvertSuccess a B.ByteString,
               MonadFailure YamlException m)
           => a
           -> m TextObject
decodeText = unsafePerformIO . decode' . convertSuccess

decodeFromText :: (ConvertSuccess a B.ByteString,
                   FromObject x Text Text)
               => a
               -> Attempt x
decodeFromText a = decodeText a >>= fromObject

decodeFromTextWrap :: (ConvertSuccess a B.ByteString,
                       FromObject x Text Text,
                       MonadFailure YamlException m,
                       MonadFailure FromObjectException m
                      )
                   => a
                   -> m x
decodeFromTextWrap a = decodeText a >>= fromObjectWrap

-- Scalar API
-- FIXME: this *should* be in a MonadFailure...
encodeScalar :: ConvertSuccess B.ByteString a
             => ScalarObject
             -> a
encodeScalar = convertSuccess . unsafePerformIO . join . encode' . toTextObject

decodeScalar :: (ConvertSuccess a B.ByteString,
                 MonadFailure YamlException m)
             => a
             -> m ScalarObject
decodeScalar = liftM toScalarObject . unsafePerformIO . decode' . convertSuccess

-- File API
encodeFile :: (MonadIO m, MonadFailure YamlException m)
           => FilePath
           -> TextObject
           -> m ()
encodeFile path o = do
    content <- liftIO $ encode' o
    content' <- content
    liftIO $ B.writeFile path content'

encodeFileToText :: (ToObject x Text Text,
                     MonadIO m,
                     MonadFailure YamlException m
                    )
                 => FilePath
                 -> x
                 -> m ()
encodeFileToText fp = encodeFile fp . toTextObject

decodeFile :: (MonadFailure YamlException m,
               MonadIO m)
           => FilePath
           -> m TextObject
decodeFile path = do
    contents <- liftIO $ B.readFile path
    join $ liftIO $ decode' contents

decodeFileFromTextWrap :: (MonadFailure YamlException m,
                           MonadFailure FromObjectException m,
                           MonadIO m,
                           FromObject x Text Text
                          )
                       => FilePath
                       -> m x
decodeFileFromTextWrap path = decodeFile path >>= fromObjectWrap

-- The real worker functions
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

#if TEST
newtype MyString = MyString String
    deriving (Eq, Show)
instance ConvertAttempt MyString Text where
    convertAttempt = return . convertSuccess
instance ConvertSuccess MyString Text where
    convertSuccess (MyString s) = convertSuccess s
instance ConvertAttempt Text MyString where
    convertAttempt = return . convertSuccess
instance ConvertSuccess Text MyString where
    convertSuccess = MyString . convertSuccess

myEquals :: Eq v => Attempt v -> Attempt v -> Bool
myEquals (Success v1) (Success v2) = v1 == v2
myEquals _ _ = False

propEncodeDecode :: Object MyString MyString -> Bool
propEncodeDecode o =
        fmap h2 (decodeText (encodeText (h1 o) :: B.ByteString))
          `myEquals` Success o
    where
        h1 = mapKeysValues convertSuccess convertSuccess
        h2 = mapKeysValues convertSuccess convertSuccess

toSBS :: ConvertSuccess a B.ByteString => a -> B.ByteString
toSBS = convertSuccess

toLBS :: ConvertSuccess a BL.ByteString => a -> BL.ByteString
toLBS = convertSuccess

caseEmptyStrings :: Assertion
caseEmptyStrings = do
    let m =
            [ ("foo", "bar")
            , ("baz", "")
            , ("bin", "")
            ]
    let m' = map (toSBS *** toSBS) m
    let m'' = map (toLBS *** toLBS) m
    let test' :: (ToObject x Text Text, FromObject x Text Text, Eq x, Show x)
              => x
              -> Assertion
        test' x = Just x @=?
                  fa (decodeText (encodeText (toTextObject x) :: String)
                        >>= fromTextObject)
    test' m
    test' m'
    test' m''

caseLargeObjects :: Assertion
caseLargeObjects = do
    let level3 = toTextObject (convertSuccess "X" :: Text)
        level2 = toTextObject $ replicate 10000 level3
        level1 = toTextObject $ zip (map (: []) ['A'..'Z']) $ repeat level2
        decoded :: Maybe TextObject
        decoded = decodeText $ (encodeText level1 :: Text)
    assertEqual "encode/decode identity" decoded $ Just level1
    putStrLn "encoding the file..."
    encodeFile "test.yaml" level1

testSuite :: Test
testSuite = testGroup "Text.Yaml"
    [ testProperty "propEncodeDecode" propEncodeDecode
    , testCase "empty strings" caseEmptyStrings
    , testCase "encode large objects" caseLargeObjects
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
