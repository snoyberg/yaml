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
      -- * Converting to/from 'TextObject's
      encodeText
    , decodeText
      -- * Convert to/from 'ScalarObejct's
    , encodeScalar
    , decodeScalar
      -- * Files
    , encodeFile
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
import Data.Object.Scalar
import System.IO.Unsafe
import Control.Arrow (first)
import Control.Monad (liftM, join)
import Control.Monad.Trans
import Data.Convertible
import Control.Monad.Failure

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit hiding (Test, path)
import Test.QuickCheck

import Control.Monad (replicateM)
import Control.Arrow ((***))
#endif

-- Low level, non-exported functions
encode' :: (ConvertSuccess B.ByteString a, MonadFailure YamlException m)
        => TextObject
        -> IO (m a)
encode' o = do
    let events = objectToEvents o
    result <- withEmitter $ flip emitEvents events
    return $ do
        result' <- result
        return $ convertSuccess result'

decode' :: (ConvertSuccess a B.ByteString,
            MonadFailure YamlException m
           )
        => a
        -> IO (m TextObject)
decode' bs = do
    events <- withParser (convertSuccess bs) parserParse
    return $ liftM eventsToTextObject events

-- Text API
-- FIXME: this *should* be in a MonadFailure
encodeText :: ConvertSuccess B.ByteString a
           => TextObject
           -> a
encodeText = unsafePerformIO . join . encode'

decodeText :: (ConvertSuccess a B.ByteString,
               MonadFailure YamlException m)
           => a
           -> m TextObject
decodeText = unsafePerformIO . decode'

-- Scalar API
-- FIXME: this *should* be in a MonadFailure...
encodeScalar :: ConvertSuccess B.ByteString a
             => ScalarObject
             -> a
encodeScalar = unsafePerformIO . join . encode' . toTextObject

decodeScalar :: (ConvertSuccess a B.ByteString,
                 MonadFailure YamlException m)
             => a
             -> m ScalarObject
decodeScalar = liftM toScalarObject . unsafePerformIO . decode'

-- File API
encodeFile :: (ToObject o Text Text, MonadIO m, MonadFailure YamlException m)
           => FilePath
           -> o
           -> m ()
encodeFile path o = do
    content <- liftIO $ encode' $ toTextObject o
    content' <- content
    liftIO $ ltWriteFile path content'

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

instance ConvertSuccess B.ByteString B.ByteString where -- FIXME belongs elsewhere
    convertSuccess = id
instance ConvertAttempt B.ByteString B.ByteString where -- FIXME belongs elsewhere
    convertAttempt = return

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

instance ToObject (Object Text Text) Text Text where -- FIXME belongs elsewhere
    toObject = id

instance ConvertSuccess B.ByteString [Char] where -- FIXME belongs elsewhere
    convertSuccess = convertSuccess . (convertSuccess :: B.ByteString -> Text)
instance ConvertAttempt B.ByteString [Char] where -- FIXME belongs elsewhere
    convertAttempt = return . convertSuccess
instance ConvertSuccess [Char] B.ByteString where -- FIXME belongs elsewhere
    convertSuccess = convertSuccess . (convertSuccess :: String -> Text)
instance ConvertAttempt [Char] B.ByteString where -- FIXME belongs elsewhere
    convertAttempt = return . convertSuccess

instance ConvertSuccess BL.ByteString [Char] where -- FIXME belongs elsewhere
    convertSuccess = convertSuccess . (convertSuccess :: BL.ByteString -> Text)
instance ConvertAttempt BL.ByteString [Char] where -- FIXME belongs elsewhere
    convertAttempt = return . convertSuccess
instance ConvertSuccess [Char] BL.ByteString where -- FIXME belongs elsewhere
    convertSuccess = convertSuccess . (convertSuccess :: String -> Text)
instance ConvertAttempt [Char] BL.ByteString where -- FIXME belongs elsewhere
    convertAttempt = return . convertSuccess

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
