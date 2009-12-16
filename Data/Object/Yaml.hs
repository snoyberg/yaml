{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Object.Yaml
    ( -- * 'YamlDoc' definition and IO
      YamlDoc (..)
    , readYamlDoc
    , writeYamlDoc
      -- * 'YamlObject' definition
    , YamlScalar (..)
    , Tag (..)
    , Style (..)
    , YamlObject
      -- * Performing conversions
    , eventsToYamlObject
      -- * Exceptions
    , YamlException (..)
      -- * Re-export
    , module Data.Object.Base
#if TEST
    , testSuite
#endif
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Object.Base
import Data.Object.Text
import Data.Object.String
import Data.Attempt

import Data.Object.Yaml.Internal
import Data.Object.Yaml.Lib
import Control.Monad ((<=<), join)
import System.IO.Unsafe

#if TEST
import Data.Object
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit hiding (Test, path)
import Test.QuickCheck

import Control.Monad (replicateM)
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Attempt
import Control.Arrow ((***))
#endif

newtype YamlDoc = YamlDoc { unYamlDoc :: ByteString }
instance ConvertSuccess [Event] YamlDoc where
    -- Note: the join here will swallow up 'YamlException's.
    -- However, these should never occur, as that would imply an error
    -- in the C library or bindings.
    convertSuccess = YamlDoc . unsafePerformIO . join . encode
instance ConvertAttempt YamlDoc [Event] where
    convertAttempt = unsafePerformIO . decode . unYamlDoc

readYamlDoc :: FilePath -> IO YamlDoc
readYamlDoc = fmap YamlDoc . B.readFile

writeYamlDoc :: FilePath -> YamlDoc -> IO ()
writeYamlDoc fp = B.writeFile fp . unYamlDoc

data YamlScalar = YamlScalar
    { value :: ByteString
    , tag :: Tag
    , style :: Style
    }
    deriving (Show, Eq)
instance ConvertSuccess YamlScalar Event where
    convertSuccess (YamlScalar v t s) = EventScalar v (convertSuccess t) s

instance ConvertSuccess YamlScalar String where
    convertSuccess = convertSuccess . value
instance ConvertSuccess String YamlScalar where
    convertSuccess t = YamlScalar (convertSuccess t) NoTag Any

instance ConvertSuccess YamlScalar Text where
    convertSuccess = convertSuccess . value
instance ConvertSuccess Text YamlScalar where
    convertSuccess t = YamlScalar (convertSuccess t) NoTag Any

instance ConvertSuccess YamlScalar ByteString where
    convertSuccess = value
instance ConvertSuccess ByteString YamlScalar where
    convertSuccess t = YamlScalar t NoTag Any

$(deriveAttempts
    [ (''String, ''YamlScalar)
    , (''YamlScalar, ''String)
    , (''ByteString, ''YamlScalar)
    , (''YamlScalar, ''ByteString)
    , (''Text, ''YamlScalar)
    , (''YamlScalar, ''Text)
    ])

-- FIXME add Scalar instances
{-
instance ConvertSuccess YamlScalar Scalar where
    convertSuccess = Text . convertSuccess
instance ConvertSuccess Scalar YamlScalar where
    convertSuccess s = convertSuccess (convertSuccess s :: Text)
-}

type YamlObject = Object YamlScalar YamlScalar

$(deriveSuccessConvs ''YamlScalar ''YamlScalar
    [''String, ''Text]
    [''String, ''Text])

$(deriveSuccessConvs ''String ''String [''YamlScalar] [''YamlScalar])
$(deriveSuccessConvs ''Text ''Text [''YamlScalar] [''YamlScalar])

-- Emit a YamlObject to an event stream
instance ConvertSuccess YamlObject [Event] where
    convertSuccess o = EventStreamStart
                     : EventDocumentStart
                     : helper o [EventDocumentEnd, EventStreamEnd] where
        helper :: YamlObject -> [Event] -> [Event]
        helper (Scalar y) rest = convertSuccess y : rest
        helper (Sequence ys) rest =
            EventSequenceStart
            : foldr ($) (EventSequenceEnd : rest) (map helper ys)
        helper (Mapping pairs) rest =
            EventMappingStart
            : foldr ($) (EventMappingEnd : rest) (map helperPairs pairs)
        helperPairs :: (YamlScalar, YamlObject) -> [Event] -> [Event]
        helperPairs (k, v) rest = convertSuccess k : helper v rest
instance ConvertSuccess YamlObject YamlDoc where
    convertSuccess = cs . (cs :: YamlObject -> [Event])
instance ConvertAttempt YamlDoc YamlObject where
    convertAttempt = ca <=< (ca :: YamlDoc -> Attempt [Event])

instance ConvertAttempt [Event] YamlObject where
    convertAttempt = eventsToYamlObject

-- | Parse a YamlObject from an event stream
eventsToYamlObject :: MonadFailure YamlException m
                   => [Event]
                   -> m YamlObject
eventsToYamlObject (EventStreamStart:EventDocumentStart:events) = h1 events
      where
        h1 :: MonadFailure YamlException m => [Event] -> m YamlObject
        h1 es = do
            (yo, es') <- h2 es
            case es' of
                [EventDocumentEnd, EventStreamEnd] -> return yo
                _ -> failure $ YamlInvalidEventStreamEnd es'
        h2 :: MonadFailure YamlException m
           => [Event]
           -> m (YamlObject, [Event])
        h2 [] = failure YamlPrematureEventStreamEnd
        h2 (EventScalar v t s:rest) =
            return (Scalar $ YamlScalar v (convertSuccess t) s, rest)
        h2 (EventSequenceStart:es) = do
            (yos, rest) <- readSeq id es
            return (Sequence yos, rest)
        h2 (EventMappingStart:es) = do
            (ypairs, rest) <- readMap id es
            return (Mapping ypairs, rest)
        h2 (e:_) = failure $ YamlInvalidStartingEvent e

        readSeq :: MonadFailure YamlException m
                => ([YamlObject] -> [YamlObject])
                -> [Event]
                -> m ([YamlObject], [Event])
        readSeq _ [] = failure YamlPrematureEventStreamEnd
        readSeq f (EventSequenceEnd:rest) = return (f [], rest)
        readSeq f es = do
            (next, rest) <- h2 es
            readSeq (f . ((:) next)) rest

        readMap :: MonadFailure YamlException m
                => ([(YamlScalar, YamlObject)] -> [(YamlScalar, YamlObject)])
                -> [Event]
                -> m ([(YamlScalar, YamlObject)], [Event])
        readMap _ [] = failure YamlPrematureEventStreamEnd
        readMap f (EventMappingEnd:rest) = return (f [], rest)
        readMap f es = do
            (key, rest) <- h2 es
            key' <- case key of
                        Scalar y -> return y
                        _ -> failure YamlNonScalarKey
            (val, rest') <- h2 rest
            readMap (f . ((:) (key', val))) rest'
eventsToYamlObject e = failure $ YamlInvalidEventStreamBeginning e

instance ConvertSuccess TextObject YamlDoc where
    convertSuccess = cs . (cs :: TextObject -> YamlObject)
instance ConvertAttempt YamlDoc TextObject where
    convertAttempt = fmap cs . (ca :: YamlDoc -> Attempt YamlObject)

instance ConvertSuccess StringObject YamlDoc where
    convertSuccess = cs . (cs :: StringObject -> YamlObject)
instance ConvertAttempt YamlDoc StringObject where
    convertAttempt = fmap cs . (ca :: YamlDoc -> Attempt YamlObject)

{- FIXME
instance ConvertSuccess (Object YamlScalar YamlScalar) (Object String Scalar) where
    convertSuccess = mapKeysValues convertSuccess convertSuccess
instance ConvertSuccess (Object String Scalar) (Object YamlScalar YamlScalar) where
    convertSuccess = mapKeysValues convertSuccess convertSuccess
-}

#if TEST
newtype MyString = MyString String
    deriving (Eq, Show)
instance ConvertSuccess MyString Text where
    convertSuccess (MyString s) = convertSuccess s
instance ConvertSuccess Text MyString where
    convertSuccess = MyString . convertSuccess
instance ConvertSuccess (Object Text Text) (Object MyString MyString) where
    convertSuccess = mapKeysValues convertSuccess convertSuccess
instance ConvertSuccess (Object MyString MyString) (Object Text Text) where
    convertSuccess = mapKeysValues convertSuccess convertSuccess

myEquals :: Eq v => Attempt v -> Attempt v -> Bool
myEquals a1 a2
    | isSuccess a1 && isSuccess a2 = fromSuccess a1 == fromSuccess a2
    | otherwise = False

propEncodeDecode :: Object MyString MyString -> Bool
propEncodeDecode o =
    let to :: TextObject
        to = cs o
        yd :: YamlDoc
        yd = cs to
        to' = ca yd
     in return to `myEquals` to'

toSBS :: ConvertSuccess a ByteString => a -> ByteString
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
    let test' :: (ConvertSuccess x TextObject,
                  ConvertSuccess TextObject x,
                  Eq x,
                  Show x)
              => x
              -> Assertion
        test' x = do
            let to :: TextObject
                to = cs x
                yd :: YamlDoc
                yd = cs to
                to' :: Attempt TextObject
                to' = ca yd
            to'' <- fa to'
            to'' @?= to
    test' $ toTextObject m
    test' $ toTextObject m'
    test' $ toTextObject m''

{-
-- FIXME remove the following
instance ToObject [(String, String)] Text Text where
    toObject = Mapping . map (cs *** toObject)
instance ToObject [(ByteString, ByteString)] Text Text where
    toObject = Mapping . map (cs *** toObject)
instance ToObject [(BL.ByteString, BL.ByteString)] Text Text where
    toObject = Mapping . map (cs *** toObject)
instance ToObject [(String, TextObject)] Text Text where
    toObject = Mapping . map (cs *** id)
-}

caseLargeObjects :: Assertion
caseLargeObjects = do
    let level3 = toTextObject (convertSuccess "X" :: Text)
        level2 = toTextObject $ replicate 10000 level3
        level1 = toTextObject $ zip (map (: []) ['A'..'Z']) $ repeat level2
        decoded :: Maybe TextObject
        decoded = fa $ ca $ (cs level1 :: YamlDoc)
    assertEqual "encode/decode identity" decoded $ Just level1
    putStrLn "encoding the file..."
    writeYamlDoc "test.yaml" $ cs level1

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
