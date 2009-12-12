{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
      -- * Converting to/from 'YamlObject's
    , encodeYaml'
    , decodeYaml'
    , encodeYaml
    , decodeYaml
    , decodeYamlSuccess
      -- * Converting to/from 'TextObject's
    , encodeText'
    , decodeText'
    , encodeText
    , decodeText
    , decodeTextSuccess
      -- * Converting to/from 'ScalarObject's
    , encodeScalar'
    , decodeScalar'
    , encodeScalar
    , decodeScalar
    , decodeScalarSuccess
#if TEST
    , testSuite
#endif
    ) where

import Prelude hiding (readList)
import Text.Libyaml
import Data.ByteString (ByteString)
import System.IO.Unsafe
import Control.Monad (join)
import Data.Convertible.Text
import Data.Attempt

import Data.Object.Text
import Data.Object.Scalar
import Data.Object.Yaml

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

-- Low level, non-exported functions
encode' :: MonadFailure YamlException m
        => YamlObject
        -> IO (m ByteString)
encode' = withEmitter . flip emitEvents . convertSuccess

decode' :: MonadFailure YamlException m
        => ByteString
        -> IO (m YamlObject)
decode' bs = do
    events <- withParser bs parserParse
    return $ events >>= eventsToYamlObject

-- Convert to YamlObject directly
encodeYaml' :: YamlObject -> ByteString
encodeYaml' = unsafePerformIO . join . encode'

decodeYaml' :: MonadFailure YamlException m => ByteString -> m YamlObject
decodeYaml' = unsafePerformIO . decode'

instance ConvertSuccess YamlObject YamlDoc where
    convertSuccess = YamlDoc . encodeYaml'
instance ConvertAttempt YamlDoc YamlObject where
    convertAttempt = decodeYaml' . unYamlDoc

-- Convert to YamlObject conversions
encodeYaml :: ConvertSuccess x YamlObject => x -> ByteString
encodeYaml = encodeYaml' . convertSuccess

decodeYaml :: ( ConvertAttempt YamlObject x
              , Failure ConversionException m
              , MonadFailure YamlException m
              )
           => ByteString
           -> m x
decodeYaml b = decodeYaml' b >>= convertAttemptWrap

decodeYamlSuccess :: ( ConvertSuccess YamlObject x
                     , MonadFailure YamlException m
                     )
                  => ByteString
                  -> m x
decodeYamlSuccess = fmap convertSuccess . decodeYaml'

-- TextObjects
encodeText' :: TextObject -> ByteString
encodeText' = encodeYaml

decodeText' :: MonadFailure YamlException m => ByteString -> m TextObject
decodeText' = decodeYamlSuccess

instance ConvertSuccess TextObject YamlDoc where
    convertSuccess = YamlDoc . encodeText'
instance ConvertAttempt YamlDoc TextObject where
    convertAttempt = decodeText' . unYamlDoc

-- TextObject conversions
encodeText :: ConvertSuccess x TextObject => x -> ByteString
encodeText = encodeText' . convertSuccess

decodeText :: ( MonadFailure YamlException m
              , ConvertAttempt TextObject x
              , Failure ConversionException m
              )
           => ByteString
           -> m x
decodeText bs = decodeText' bs >>= convertAttemptWrap

decodeTextSuccess :: ( MonadFailure YamlException m
                     , ConvertSuccess TextObject x
                     )
                  => ByteString
                  -> m x
decodeTextSuccess = fmap convertSuccess . decodeText'

-- ScalarObjects
encodeScalar' :: ScalarObject -> ByteString
encodeScalar' = encodeYaml

decodeScalar' :: MonadFailure YamlException m
              => ByteString
              -> m ScalarObject
decodeScalar' = decodeYamlSuccess

instance ConvertSuccess ScalarObject YamlDoc where
    convertSuccess = YamlDoc . encodeScalar'
instance ConvertAttempt YamlDoc ScalarObject where
    convertAttempt = decodeScalar' . unYamlDoc

-- ScalarObject conversions
encodeScalar :: ConvertSuccess x ScalarObject => x -> ByteString
encodeScalar = encodeScalar' . convertSuccess

decodeScalar :: ( MonadFailure YamlException m
              , ConvertAttempt ScalarObject x
              , Failure ConversionException m
              )
           => ByteString
           -> m x
decodeScalar bs = decodeScalar' bs >>= convertAttemptWrap

decodeScalarSuccess :: ( MonadFailure YamlException m
                     , ConvertSuccess ScalarObject x
                     )
                  => ByteString
                  -> m x
decodeScalarSuccess = fmap convertSuccess . decodeScalar'

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
        (decodeTextSuccess $ encodeText' $ convertSuccess o)
          `myEquals` return o

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
            let bs = encodeText x
            x' <- decodeTextSuccess bs
            x' @?= x
    test' $ toTextObject m
    test' $ toTextObject m'
    test' $ toTextObject m''

caseLargeObjects :: Assertion
caseLargeObjects = do
    let level3 = toTextObject (convertSuccess "X" :: Text)
        level2 = toTextObject $ replicate 10000 level3
        level1 = toTextObject $ zip (map (: []) ['A'..'Z']) $ repeat level2
        decoded :: Maybe TextObject
        decoded = decodeText $ encodeText level1
    assertEqual "encode/decode identity" decoded $ Just level1
    putStrLn "encoding the file..."
    B.writeFile "test.yaml" $ encodeText' level1

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
