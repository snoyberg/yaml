{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.YamlSpec (main, spec) where

import qualified Text.Libyaml as Y
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Int (Int64)
import qualified Data.Scientific as S

import Test.HUnit hiding (Test, path)

import Data.Conduit (runConduitRes, (.|), ConduitM)
import qualified Data.Conduit.List as CL

import Control.Monad
import Control.Exception (try, SomeException)
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Either.Compat
import System.Directory (createDirectory, createDirectoryIfMissing)
import Test.Mockery.Directory

import qualified Data.Yaml as D
import qualified Data.Yaml.Builder as B
import qualified Data.Yaml.Internal as Internal
import qualified Data.Yaml.Pretty as Pretty
import Data.Yaml (object, array, (.=))
import Data.Maybe
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Data.Aeson.TH
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

data TestJSON = TestJSON
              { string :: Text
              , number :: Int
              , anArray  :: Vector Text
              , hash   :: HashMap Text Text
              , extrastring :: Text
              } deriving (Show, Eq)

deriveJSON defaultOptions ''TestJSON

testJSON :: TestJSON
testJSON = TestJSON
         { string = "str"
         , number = 2
         , anArray = V.fromList ["a", "b"]
         , hash = HM.fromList [("key1", "value1"), ("key2", "value2")]
         , extrastring = "1234-foo"
         }

shouldDecode :: (Show a, D.FromJSON a, Eq a) => B8.ByteString -> a -> IO ()
shouldDecode bs expected = do
    actual <- D.decodeThrow bs
    actual `shouldBe` expected

shouldDecodeEvents :: B8.ByteString -> [Y.Event] -> IO ()
shouldDecodeEvents bs expected = do
    actual <- runConduitRes $ Y.decode bs .| CL.consume
    map anyStyle actual `shouldBe` map anyStyle expected

anyStyle :: Y.Event -> Y.Event
anyStyle (Y.EventScalar bs tag _ anchor)     = Y.EventScalar bs tag Y.Any anchor
anyStyle (Y.EventSequenceStart tag _ anchor) = Y.EventSequenceStart tag Y.AnySequence anchor
anyStyle (Y.EventMappingStart tag _ anchor)  = Y.EventMappingStart tag Y.AnyMapping anchor
anyStyle event = event

testEncodeWith :: Y.FormatOptions -> [Y.Event] -> IO BS.ByteString
testEncodeWith opts es = runConduitRes $ CL.sourceList (eventStream es) .| Y.encodeWith opts

eventStream :: [Y.Event] -> [Y.Event]
eventStream events =
    [Y.EventStreamStart, Y.EventDocumentStart]
    ++ events
    ++ [Y.EventDocumentEnd, Y.EventStreamEnd]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "streaming" $ do
        it "count scalars with anchor" caseCountScalarsWithAnchor
        it "count sequences with anchor" caseCountSequencesWithAnchor
        it "count mappings with anchor" caseCountMappingsWithAnchor
        it "count sequences with custom tag" caseCountSequenceTags
        it "count mappings with custom tag" caseCountMappingTags
        it "count count sequences with ! tag" caseCountEmptySequenceTags
        it "count count mappings with ! tag" caseCountEmptyMappingTags
        it "count block style sequences" caseCountBlockStyleSequences
        it "count flow style sequences" caseCountFlowStyleSequences
        it "count block style mappings" caseCountBlockStyleMappings
        it "count flow style mappings" caseCountFlowStyleMappings
        it "count aliases" caseCountAliases
        it "count scalars" caseCountScalars
        it "largest string" caseLargestString
        it "encode/decode" caseEncodeDecode
        it "encode/decode events" caseEncodeDecodeEvents
        it "encode/decode file" caseEncodeDecodeFile
        it "interleaved encode/decode" caseInterleave
        it "decode invalid document (without segfault)" caseDecodeInvalidDocument
    describe "Data.Yaml" $ do
        it "encode/decode" caseEncodeDecodeData
        it "encode/decode file" caseEncodeDecodeFileData
        it "encode/decode files with non-ASCII names" caseEncodeDecodeNonAsciiFileData
        it "encode/decode strings" caseEncodeDecodeStrings
        it "decode invalid file" caseDecodeInvalid
        it "processes datatypes" caseDataTypes
        it "encode invalid numbers" caseEncodeInvalidNumbers
    describe "Data.Yaml.Pretty" $ do
        it "encode/decode" caseEncodeDecodeDataPretty
        it "encode/decode strings" caseEncodeDecodeStringsPretty
        it "processes datatypes" caseDataTypesPretty
    describe "Data.Yaml.Builder" $ do
        it "encode/decode" caseEncodeDecodeDataBuilder
        it "encode/decode complex mapping" caseEncodeDecodeComplexMappingBuilder
    describe "Data.Yaml aliases" $ do
        it "simple scalar alias" caseSimpleScalarAlias
        it "simple sequence alias" caseSimpleSequenceAlias
        it "simple mapping alias" caseSimpleMappingAlias
        it "mapping alias before anchor" caseMappingAliasBeforeAnchor
        it "mapping alias inside anchor" caseMappingAliasInsideAnchor
        it "scalar alias overriding" caseScalarAliasOverriding
    describe "Data.Yaml merge keys" $ do
        it "test uniqueness of keys" caseAllKeysShouldBeUnique
        it "test mapping merge" caseSimpleMappingMerge
        it "test sequence of mappings merging" caseMergeSequence
    describe "numbers" $ do
        it "parses as string when quoted" caseQuotedNumber
        it "parses as number when unquoted" caseUnquotedNumber
        it "parses as number !!str is present" caseAttribNumber
        it "integers have no decimals" caseIntegerDecimals
    describe "booleans" $ do
        it "parses when all lowercase" caseLowercaseBool
        it "parses when all uppercase" caseUppercaseBool
        it "parses when titlecase" caseTitlecaseBool
    describe "empty input" $ do
        it "parses as Null" caseEmptyInput
        it "parses as Null from file" caseEmptyInputFile
    describe "comment only input" $ do
        it "parses as Null" caseCommentOnlyInput
        it "parses as Null from file" caseCommentOnlyInputFile
    describe "alias stripping" $ do
        it "works" caseStripAlias
    describe "nulls" $ do
        checkNull "null"
        checkNull "Null"
        checkNull "NULL"
        checkNull "~"
        checkNull ""

    describe "pretty output" $ do
        it "simple nulls" $ D.encode (object ["foo" .= D.Null]) `shouldBe` "foo: null\n"
        it "simple numbers" $ D.encode (object ["foo" .= (4 :: Int)]) `shouldBe` "foo: 4\n"
        it "True" $ D.encode (object ["foo" .= True]) `shouldBe` "foo: true\n"
        it "False" $ D.encode (object ["foo" .= False]) `shouldBe` "foo: false\n"
        it "simple string" $ D.encode (object ["foo" .= ("bar" :: T.Text)]) `shouldBe` "foo: bar\n"
        it "*" $ D.encode (object ["foo" .= ("*" :: T.Text)]) `shouldBe` "foo: '*'\n"

    describe "special keys" $ do
        let tester key = it (T.unpack key) $
                let value = object [key .= True]
                 in D.encode value `shouldDecode` value
        mapM_ tester specialStrings

    describe "special values" $ do
        let tester value = it (T.unpack value) $
                let value' = object ["foo" .= value]
                 in D.encode value' `shouldDecode` value'
        mapM_ tester specialStrings

    describe "decodeFileEither" $ do
        it "loads YAML through JSON into Haskell data" $ do
          tj <- either (error . show) id `fmap` D.decodeFileEither "test/json.yaml"
          tj `shouldBe` testJSON

        context "when file does not exist" $ do
            it "returns Left" $ do
                (D.decodeFileEither "./does_not_exist.yaml" :: IO (Either D.ParseException D.Value)) >>= (`shouldSatisfy` isLeft)


    describe "round-tripping of special scalars" $ do
        let special = words "y Y On ON false 12345 12345.0 12345a 12e3"
        forM_ special $ \w -> it w $
            let v = object ["word" .= w]
             in D.encode v `shouldDecode` v
        it "no tags" $ D.encode (object ["word" .= ("true" :: String)]) `shouldBe` "word: 'true'\n"

    it "aliases in keys #49" caseIssue49

    it "serialization of +123 #64" $ do
        D.encode ("+123" :: String) `shouldDecode` ("+123" :: String)

    it "preserves Scientific precision" casePreservesScientificPrecision

    it "truncates files" caseTruncatesFiles

    it "encode quotes special keys #137" $ caseSpecialKeys D.encode

    it "encodePretty quotes special keys #179" $ caseSpecialKeys (Pretty.encodePretty Pretty.defConfig)

    describe "non-decimal numbers #135" $ do
      let go str val = it str $ encodeUtf8 (T.pack str) `shouldDecode` val
      go "12345" (12345 :: Int)
      go "+12345" (12345 :: Int)
      go "0o14" (12 :: Int)
      go "0o123" (83 :: Int)
      go "0xC" (12 :: Int)
      go "0xc" (12 :: Int)
      go "0xdeadBEEF" (3735928559 :: Int64)
      go "0xDEADBEEF" (3735928559 :: Int64)
      go "1.23015e+3" (1230.15 :: Scientific)
      go "12.3015e+02" (1230.15 :: Scientific)
      go "1230.15" (1230.15 :: Scientific)

    describe "Text.Libyaml with default tag rendering" $ do
      let enc = testEncodeWith Y.defaultFormatOptions
      it "elides custom sequence tags" $
        enc taggedSequence `shouldReturn` "[]\n"
      it "elides custom mapping tags" $
        enc taggedMapping `shouldReturn` "{}\n"
      it "elides default sequence tags" $
        enc defaultTaggedSequence `shouldReturn` "[]\n"
      it "elides default mapping tags" $
        enc defaultTaggedMapping `shouldReturn` "{}\n"
      it "handles NoTag on sequences" $
        enc untaggedSequence `shouldReturn` "[]\n"
      it "handles NoTag on mappings" $
        enc untaggedMapping `shouldReturn` "{}\n"
      it "handles mixed tag usages but elides all mapping and sequence tags" $
        enc mixedTagSampleA `shouldReturn` "- {}\n"
      it "in combination of tags, anchors and styles, outputs only the scalar tags" $
        enc mixedTagSampleB `shouldReturn` "&a\n&b !<bar> foo: &c [&d !!null '']\n"
      it "outputs tags when double quoted" $
        enc [Y.EventScalar "foo" Y.StrTag Y.DoubleQuoted Nothing] `shouldReturn` "!!str \"foo\"\n"
      it "outputs tags when single quoted" $
        enc [Y.EventScalar "foo" Y.StrTag Y.SingleQuoted Nothing] `shouldReturn` "!!str 'foo'\n"
      it "outputs tags on literal text" $
        enc [Y.EventScalar "foo" Y.StrTag Y.Literal Nothing] `shouldReturn` "!!str |-\n  foo\n"
      it "outputs tags on folded text" $
        enc [Y.EventScalar "foo" Y.StrTag Y.Folded Nothing] `shouldReturn` "!!str >-\n  foo\n"
    describe "Text.Libyaml with all tags on" $ do
      let enc = testEncodeWith $ Y.setTagRendering Y.renderAllTags Y.defaultFormatOptions
      it "will output custom sequence tags" $
        enc taggedSequence `shouldReturn` "!foo []\n"
      it "will output custom mapping tags" $
        enc taggedMapping `shouldReturn` "!foo {}\n"
      it "will output default sequence tags" $
        enc defaultTaggedSequence `shouldReturn` "!!seq []\n"
      it "will output default mapping tags" $
        enc defaultTaggedMapping `shouldReturn` "!!map {}\n"
      it "handles NoTag on sequences" $
        enc untaggedSequence `shouldReturn` "[]\n"
      it "handles NoTag on mappings" $
        enc untaggedMapping `shouldReturn` "{}\n"
      it "handles mixed tag usages outputting all mapping and sequence tags" $
        enc mixedTagSampleA `shouldReturn` "- !foo {}\n"
      it "in combination of tags, anchors and styles, outputs all the tags" $
        enc mixedTagSampleB `shouldReturn` "&a\n&b !<bar> foo: &c !baz [&d !!null '']\n"
      it "outputs plain tags" $
        enc [Y.EventScalar "foo" Y.StrTag Y.Plain Nothing] `shouldReturn` "!!str foo\n"
      it "respects PlainNoTag tags" $
        enc [Y.EventScalar "foo" Y.StrTag Y.PlainNoTag Nothing] `shouldReturn` "foo\n"
    describe "Text.Libyaml with uri tags on" $ do
      let enc = testEncodeWith $ Y.setTagRendering Y.renderUriTags Y.defaultFormatOptions
      it "will output custom sequence tags" $
        enc taggedSequence `shouldReturn` "!foo []\n"
      it "will output custom mapping tags" $
        enc taggedMapping `shouldReturn` "!foo {}\n"
      it "will output default sequence tags" $
        enc defaultTaggedSequence `shouldReturn` "[]\n"
      it "will output default mapping tags" $
        enc defaultTaggedMapping `shouldReturn` "{}\n"
      it "handles NoTag on sequences" $
        enc untaggedSequence `shouldReturn` "[]\n"
      it "handles NoTag on mappings" $
        enc untaggedMapping `shouldReturn` "{}\n"
      it "handles mixed tag usages outputting all mapping and sequence tags" $
        enc mixedTagSampleA `shouldReturn` "- !foo {}\n"
      it "in combination of tags, anchors and styles, outputs all the tags" $
        enc mixedTagSampleB `shouldReturn` "&a\n&b !<bar> foo: &c !baz [&d '']\n"
    describe "Text.Libyaml with tags off" $ do
      let enc = testEncodeWith $ Y.setTagRendering Y.renderNoTags Y.defaultFormatOptions
      it "outputs plain tags" $
        enc [Y.EventScalar "foo" Y.StrTag Y.Plain Nothing] `shouldReturn` "foo\n"
      it "respects PlainNoTag tags" $
        enc [Y.EventScalar "foo" Y.StrTag Y.PlainNoTag Nothing] `shouldReturn` "foo\n"
      it "elides tags when double quoted" $
        enc [Y.EventScalar "foo" Y.StrTag Y.DoubleQuoted Nothing] `shouldReturn` "\"foo\"\n"
      it "elides tags when single quoted" $
        enc [Y.EventScalar "foo" Y.StrTag Y.SingleQuoted Nothing] `shouldReturn` "'foo'\n"
      it "elides tags on literal text" $
        enc [Y.EventScalar "foo" Y.StrTag Y.Literal Nothing] `shouldReturn` "|-\n  foo\n"
      it "elides tags on folded text" $
        enc [Y.EventScalar "foo" Y.StrTag Y.Folded Nothing] `shouldReturn` ">-\n  foo\n"
    describe "Text.Libyaml with only UriTags set to render " $ do
      let enc =
            testEncodeWith $
            Y.setTagRendering Y.renderUriTags $ Y.defaultFormatOptions
      it "outputs only UriTags" $
        enc
          [ Y.EventSequenceStart Y.NoTag Y.FlowSequence Nothing
          , Y.EventScalar "foo" Y.StrTag Y.DoubleQuoted Nothing
          , Y.EventScalar "99" Y.IntTag Y.Plain Nothing
          , Y.EventScalar "99.99" Y.FloatTag Y.Plain Nothing
          , Y.EventScalar "bar" Y.NoTag Y.Plain Nothing
          , Y.EventScalar "foo" (Y.UriTag "!foo") Y.DoubleQuoted Nothing
          , Y.EventScalar "foo" (Y.UriTag "!foo") Y.Plain Nothing
          , Y.EventSequenceEnd
          ] `shouldReturn`
        "[\"foo\", 99, 99.99, bar, !foo \"foo\", !foo foo]\n"

      prop "Scientific values round-trip" $ \coeff expon -> do
        let val = D.Number $ S.scientific coeff expon
        let rendered = D.encode val
        case D.decodeEither' rendered of
          Left e -> error $ show (coeff, expon, e)
          Right val' -> val' `shouldBe` val

specialStrings :: [T.Text]
specialStrings =
    [ "fo\"o"
    , "fo\'o"
    , "fo\\'o"
    , "fo: o"
    , "foo\nbar\nbaz\n"
    ]

counter :: Monad m => (Y.Event -> Bool) -> ConduitM Y.Event o m Int
counter pred' =
    CL.fold (\cnt e -> (if pred' e then 1 else 0) + cnt) 0

caseHelper :: String
           -> (Y.Event -> Bool)
           -> Int
           -> Assertion
caseHelper yamlString pred' expRes = do
    res <- runConduitRes $ Y.decode (B8.pack yamlString) .| counter pred'
    res @?= expRes

caseCountScalarsWithAnchor :: Assertion
caseCountScalarsWithAnchor =
    caseHelper yamlString isScalarA 1
  where
    yamlString = "foo:\n  - &anchor bin1\n  - bin2\n  - bin3"
    isScalarA (Y.EventScalar _ _ _ (Just _)) = True
    isScalarA _ = False

caseCountSequencesWithAnchor :: Assertion
caseCountSequencesWithAnchor =
    caseHelper yamlString isSequenceStartA 1
  where
    yamlString = "foo: &anchor\n  - bin1\n  - bin2\n  - bin3"
    isSequenceStartA (Y.EventSequenceStart Y.NoTag _ (Just _)) = True
    isSequenceStartA _ = False

caseCountMappingsWithAnchor :: Assertion
caseCountMappingsWithAnchor =
    caseHelper yamlString isMappingA 1
  where
    yamlString = "foo: &anchor\n  key1: bin1\n  key2: bin2\n  key3: bin3"
    isMappingA (Y.EventMappingStart _ _ (Just _)) = True
    isMappingA _ = False

caseCountAliases :: Assertion
caseCountAliases =
    caseHelper yamlString isAlias 1
  where
    yamlString = "foo: &anchor\n  key1: bin1\n  key2: bin2\n  key3: bin3\nboo: *anchor"
    isAlias Y.EventAlias{} = True
    isAlias _ = False

caseCountMappingTags :: Assertion
caseCountMappingTags =
    caseHelper yamlString isCustomTaggedMapping 1
  where
    yamlString = "foo: !bar\n  k: v\n  k2: v2"
    isCustomTaggedMapping (Y.EventMappingStart (Y.UriTag "!bar") _ _) = True
    isCustomTaggedMapping _ = False

caseCountEmptyMappingTags :: Assertion
caseCountEmptyMappingTags =
    caseHelper yamlString isCustomTaggedMapping 1
  where
    yamlString = "foo: !\n  k: v\n  k2: v2"
    isCustomTaggedMapping (Y.EventMappingStart (Y.UriTag "!") _ _) = True
    isCustomTaggedMapping _ = False

caseCountSequenceTags :: Assertion
caseCountSequenceTags =
    caseHelper yamlString isCustomTaggedSequence 1
  where
    yamlString = "foo: !bar [x, y, z]"
    isCustomTaggedSequence (Y.EventSequenceStart (Y.UriTag "!bar") _ _) = True
    isCustomTaggedSequence _ = False

caseCountEmptySequenceTags :: Assertion
caseCountEmptySequenceTags =
    caseHelper yamlString isCustomTaggedSequence 1
  where
    yamlString = "foo: ! [x, y, z]"
    isCustomTaggedSequence (Y.EventSequenceStart (Y.UriTag "!") _ _) = True
    isCustomTaggedSequence _ = False

caseCountFlowStyleSequences :: Assertion
caseCountFlowStyleSequences =
    caseHelper yamlString isFlowStyleSequence 1
  where
    yamlString = "foo: [x, y, z]"
    isFlowStyleSequence (Y.EventSequenceStart _ Y.FlowSequence _) = True
    isFlowStyleSequence _ = False

caseCountBlockStyleSequences :: Assertion
caseCountBlockStyleSequences =
    caseHelper yamlString isBlockStyleSequence 1
  where
    yamlString = "foo:\n- x\n- y\n- z\n"
    isBlockStyleSequence (Y.EventSequenceStart _ Y.BlockSequence _) = True
    isBlockStyleSequence _ = False

caseCountFlowStyleMappings :: Assertion
caseCountFlowStyleMappings =
    caseHelper yamlString isFlowStyleMapping 1
  where
    yamlString = "foo: { bar: 1, baz: 2 }"
    isFlowStyleMapping (Y.EventMappingStart _ Y.FlowMapping _) = True
    isFlowStyleMapping _ = False

caseCountBlockStyleMappings :: Assertion
caseCountBlockStyleMappings =
    caseHelper yamlString isBlockStyleMapping 1
  where
    yamlString = "foo: bar\nbaz: quux"
    isBlockStyleMapping (Y.EventMappingStart _ Y.BlockMapping _) = True
    isBlockStyleMapping _ = False

caseCountScalars :: Assertion
caseCountScalars = do
    res <- runConduitRes $ Y.decode yamlBS .| CL.fold adder accum
    res @?= (7, 1, 2)
  where
    yamlString = "foo:\n  baz: [bin1, bin2, bin3]\nbaz: bazval"
    yamlBS = B8.pack yamlString
    adder (s, l, m) (Y.EventScalar{})        = (s + 1, l, m)
    adder (s, l, m) (Y.EventSequenceStart{}) = (s, l + 1, m)
    adder (s, l, m) (Y.EventMappingStart{})  = (s, l, m + 1)
    adder a         _                      = a
    accum = (0, 0, 0) :: (Int, Int, Int)

caseLargestString :: Assertion
caseLargestString = do
    res <- runConduitRes $ Y.decodeFile filePath .| CL.fold adder accum
    res @?= (length expected, expected)
    where
        expected = "this one is just a little bit bigger than the others"
        filePath = "test/largest-string.yaml"
        adder (i, s) (Y.EventScalar bs _ _ _) =
            let s' = B8.unpack bs
                i' = length s'
             in if i' > i then (i', s') else (i, s)
        adder acc _ = acc
        accum = (0, "no strings found")

newtype MyEvent = MyEvent Y.Event deriving Show
instance Eq MyEvent where
    (MyEvent (Y.EventScalar s t _ _)) == (MyEvent (Y.EventScalar s' t' _ _)) =
        s == s' && t == t'
    MyEvent e1 == MyEvent e2 = e1 == e2

caseEncodeDecode :: Assertion
caseEncodeDecode = do
    eList <- runConduitRes $ Y.decode yamlBS .| CL.consume
    bs <- runConduitRes $ CL.sourceList eList .| Y.encode
    eList2 <- runConduitRes $ Y.decode bs .| CL.consume
    map MyEvent eList @=? map MyEvent eList2
  where
    yamlString = "foo: bar\nbaz:\n - bin1\n - bin2\n"
    yamlBS = B8.pack yamlString

caseEncodeDecodeEvents :: Assertion
caseEncodeDecodeEvents = do
    let events = Internal.objToEvents D.defaultStringStyle testJSON []
    result <- Internal.decodeHelper_ . CL.sourceList $ eventStream events
    let (_, value) = either (error . show) id result
    value @?= testJSON

caseEncodeDecodeFile :: Assertion
caseEncodeDecodeFile = withFile "" $ \tmpPath -> do
    eList <- runConduitRes $ Y.decodeFile filePath .| CL.consume
    runConduitRes $ CL.sourceList eList .| Y.encodeFile tmpPath
    eList2 <- runConduitRes $ Y.decodeFile filePath .| CL.consume
    map MyEvent eList @=? map MyEvent eList2
  where
    filePath = "test/largest-string.yaml"

caseInterleave :: Assertion
caseInterleave = withFile "" $ \tmpPath -> withFile "" $ \tmpPath2 -> do
    () <- runConduitRes $ Y.decodeFile filePath .| Y.encodeFile tmpPath
    () <- runConduitRes $ Y.decodeFile tmpPath .| Y.encodeFile tmpPath2
    f1 <- readFile tmpPath
    f2 <- readFile tmpPath2
    f1 @=? f2
  where
    filePath = "test/largest-string.yaml"

caseDecodeInvalidDocument :: Assertion
caseDecodeInvalidDocument = do
    x <- try $ runConduitRes $ Y.decode yamlBS .| CL.sinkNull
    case x of
        Left (_ :: SomeException) -> return ()
        Right y -> do
            putStrLn $ "bad return value: " ++ show y
            assertFailure "expected parsing exception, but got no errors"
  where
    yamlString = "  - foo\n  - baz\nbuz"
    yamlBS = B8.pack yamlString

mkScalar :: String -> D.Value
mkScalar = mkStrScalar

mkStrScalar :: String -> D.Value
mkStrScalar = D.String . T.pack

mappingKey :: D.Value-> String -> D.Value
mappingKey (D.Object m) k = (fromJust . M.lookup (T.pack k) $ m)
mappingKey _ _ = error "expected Object"

sample :: D.Value
sample = array
    [ D.String "foo"
    , object
        [ ("bar1", D.String "bar2")
        , ("bar3", D.String "")
        ]
    , D.String ""
    , D.Number 1
    , D.Number 0.1
    , D.Bool True
    , D.Null
    ]

sampleBuilder :: B.YamlBuilder
sampleBuilder = B.array
    [ B.string "foo"
    , B.mapping
        [ ("bar1", B.string "bar2")
        , ("bar3", B.string "")
        ]
    , B.string ""
    , B.scientific 1
    , B.scientific 0.1
    , B.bool True
    , B.null
    ]

caseEncodeDecodeData :: Assertion
caseEncodeDecodeData = D.encode sample `shouldDecode` sample

caseEncodeDecodeDataPretty :: Assertion
caseEncodeDecodeDataPretty =
    Pretty.encodePretty Pretty.defConfig sample `shouldDecode` sample

caseEncodeDecodeDataBuilder :: Assertion
caseEncodeDecodeDataBuilder = do
    let events = B.unYamlBuilder sampleBuilder []
    bs <- testEncodeWith Y.defaultFormatOptions events
    bs `shouldDecodeEvents` eventStream events

caseEncodeDecodeComplexMappingBuilder :: Assertion
caseEncodeDecodeComplexMappingBuilder = do
    let events = B.unYamlBuilder builder []
    bs <- testEncodeWith Y.defaultFormatOptions events
    bs `shouldDecodeEvents` eventStream events
  where
    builder :: B.YamlBuilder
    builder = B.mappingComplex
        [ ( B.mapping
              [ ("foo", B.scientific 1)
              , ("bar", B.scientific 2)
              ]
          , B.bool True
          )
        ]

caseEncodeDecodeFileData :: Assertion
caseEncodeDecodeFileData = withFile "" $ \fp -> do
    D.encodeFile fp sample
    out <- D.decodeFileThrow fp
    out @?= sample

caseEncodeDecodeNonAsciiFileData :: Assertion
caseEncodeDecodeNonAsciiFileData = do
  let mySample = (object ["foo" .= True])
  inTempDirectory $ do
    createDirectory "accenté"
    D.encodeFile "accenté/bar.yaml" mySample
    out1 <- D.decodeFileThrow "accenté/bar.yaml"
    out1 @?= mySample

  c <- readFile "test/resources/accent/foo.yaml"
  inTempDirectory $ do
    createDirectoryIfMissing True "test/resources/unicode/accenté/"
    writeFile "test/resources/unicode/accenté/foo.yaml" c
    out2 <- D.decodeFileThrow "test/resources/unicode/accenté/foo.yaml"
    out2 @?= mySample

caseEncodeDecodeStrings :: Assertion
caseEncodeDecodeStrings = D.encode sample `shouldDecode` sample

caseEncodeDecodeStringsPretty :: Assertion
caseEncodeDecodeStringsPretty =
    Pretty.encodePretty Pretty.defConfig sample `shouldDecode` sample

caseDecodeInvalid :: Assertion
caseDecodeInvalid =
    D.decodeThrow "\tthis is 'not' valid :-)" `shouldBe`
    (Nothing :: Maybe D.Value)

caseSimpleScalarAlias :: Assertion
caseSimpleScalarAlias =
    "- &anch foo\n- baz\n- *anch" `shouldDecode`
    array [(mkScalar "foo"), (mkScalar "baz"), (mkScalar "foo")]

caseSimpleSequenceAlias :: Assertion
caseSimpleSequenceAlias =
    "seq: &anch\n  - foo\n  - baz\nseq2: *anch" `shouldDecode`
    object [("seq", array [(mkScalar "foo"), (mkScalar "baz")]), ("seq2", array [(mkScalar "foo"), (mkScalar "baz")])]

caseSimpleMappingAlias :: Assertion
caseSimpleMappingAlias =
    "map: &anch\n  key1: foo\n  key2: baz\nmap2: *anch" `shouldDecode`
    object [(T.pack "map", object [("key1", mkScalar "foo"), ("key2", (mkScalar "baz"))]), (T.pack "map2", object [("key1", (mkScalar "foo")), ("key2", mkScalar "baz")])]

caseMappingAliasBeforeAnchor :: Assertion
caseMappingAliasBeforeAnchor =
    case D.decodeThrow "map: *anch\nmap2: &anch\n  key1: foo\n  key2: baz" of
      Nothing -> pure ()
      Just (_ :: D.Value) -> error "decode should return Nothing due to unknown alias"

caseMappingAliasInsideAnchor :: Assertion
caseMappingAliasInsideAnchor = do
    case D.decodeThrow "map: &anch\n  key1: foo\n  key2: *anch" of
      Nothing -> pure ()
      Just (_ :: D.Value) -> error "decode should return Nothing due to unknown alias"

caseScalarAliasOverriding :: Assertion
caseScalarAliasOverriding =
    "- &anch foo\n- baz\n- *anch\n- &anch boo\n- buz\n- *anch" `shouldDecode`
    array [(mkScalar "foo"), (mkScalar "baz"), (mkScalar "foo"), (mkScalar "boo"), (mkScalar "buz"), (mkScalar "boo")]

caseAllKeysShouldBeUnique :: Assertion
caseAllKeysShouldBeUnique = do
    res <- D.decodeThrow "foo1: foo\nfoo2: baz\nfoo1: buz"
    mappingKey res "foo1" `shouldBe` mkScalar "buz"

caseSimpleMappingMerge :: Assertion
caseSimpleMappingMerge = do
    res <- D.decodeThrow "foo1: foo\nfoo2: baz\n<<:\n  foo1: buz\n  foo3: fuz"
    mappingKey res "foo1" `shouldBe` mkScalar "foo"
    mappingKey res "foo3" `shouldBe` mkScalar "fuz"

caseMergeSequence :: Assertion
caseMergeSequence = do
    res <- D.decodeThrow "m1: &m1\n  k1: !!str 1\n  k2: !!str 2\nm2: &m2\n  k1: !!str 3\n  k3: !!str 4\nfoo1: foo\n<<: [ *m1, *m2 ]"
    mappingKey res "foo1" @?= (mkScalar "foo")
    mappingKey res "k1" @?= (D.String "1")
    mappingKey res "k2" @?= (D.String "2")
    mappingKey res "k3" @?= (D.String "4")

caseDataTypes :: Assertion
caseDataTypes =
    D.encode val `shouldDecode` val
  where
    val = object
        [ ("string", D.String "foo")
        , ("int", D.Number 5)
        , ("float", D.Number 4.3)
        , ("true", D.Bool True)
        , ("false", D.Bool False)
        , ("null", D.Null)
        ]

caseEncodeInvalidNumbers :: Assertion
caseEncodeInvalidNumbers =
    D.encode (D.String ".") `shouldBe` ".\n"

caseDataTypesPretty :: Assertion
caseDataTypesPretty =
    Pretty.encodePretty Pretty.defConfig val `shouldDecode` val
  where
    val = object
        [ ("string", D.String "foo")
        , ("int", D.Number 5)
        , ("float", D.Number 4.3)
        , ("true", D.Bool True)
        , ("false", D.Bool False)
        , ("null", D.Null)
        ]
caseQuotedNumber, caseUnquotedNumber, caseAttribNumber, caseIntegerDecimals :: Assertion
caseQuotedNumber = "foo: \"1234\"" `shouldDecode` object [("foo", D.String "1234")]
caseUnquotedNumber = "foo: 1234" `shouldDecode` object [("foo", D.Number 1234)]
caseAttribNumber = "foo: !!str 1234" `shouldDecode` object [("foo", D.String "1234")]
caseIntegerDecimals = "1\n" `shouldDecode` (1 :: Int)

obj :: D.Value
obj = object [("foo", D.Bool False), ("bar", D.Bool True), ("baz", D.Bool True)]

caseLowercaseBool, caseUppercaseBool, caseTitlecaseBool :: Assertion
caseLowercaseBool = "foo: off\nbar: y\nbaz: true" `shouldDecode` obj
caseUppercaseBool = "foo: FALSE\nbar: Y\nbaz: ON" `shouldDecode` obj
caseTitlecaseBool = "foo: No\nbar: Yes\nbaz: True" `shouldDecode` obj

caseEmptyInput :: Assertion
caseEmptyInput = B8.empty `shouldDecode` D.Null

caseEmptyInputFile :: Assertion
caseEmptyInputFile = do
    out <- D.decodeFileEither "test/resources/empty.yaml"
    either (Left . D.prettyPrintParseException) Right out @?= Right D.Null

caseCommentOnlyInput :: Assertion
caseCommentOnlyInput = "# comment\n" `shouldDecode` D.Null

caseCommentOnlyInputFile :: Assertion
caseCommentOnlyInputFile = do
    out <- D.decodeFileEither "test/resources/empty2.yaml"
    either (Left . D.prettyPrintParseException) Right out @?= Right D.Null

checkNull :: T.Text -> Spec
checkNull x =
    it ("null recognized: " ++ show x) $
      B8.pack ("foo: " ++ T.unpack x) `shouldDecode` object [("foo", D.Null)]

caseStripAlias :: Assertion
caseStripAlias =
    src `shouldDecode` object
        [ "Default" .= object
            [ "foo" .= (1 :: Int)
            , "bar" .= (2 :: Int)
            ]
        , "Obj" .= object
            [ "foo" .= (1 :: Int)
            , "bar" .= (2 :: Int)
            , "key" .= (3 :: Int)
            ]
        ]
  where
    src = "Default: &def\n  foo: 1\n  bar: 2\nObj:\n  <<: *def\n  key: 3\n"

caseIssue49 :: Assertion
caseIssue49 =
    src `shouldDecode` object
        [ "a" .= object [ "value" .= (1.0 :: Double) ]
        , "b" .= object [ "value" .= (1.2 :: Double) ]
        ]
  where
    src = "---\na:\n  &id5 value: 1.0\nb:\n  *id5: 1.2"

-- | We cannot guarantee this before aeson started using 'Scientific'.
casePreservesScientificPrecision :: Assertion
casePreservesScientificPrecision = do
    "x: 1e-100000" `shouldDecode` object
        [ "x" .= D.Number (read "1e-100000") ]
    -- Note that this ought to work also without 'Scientific', given
    -- that @read (show "9.78159610558926e-5") == 9.78159610558926e-5@.
    -- However, it didn't work (and still doesn't work with aeson < 0.7)
    -- for two reasons:
    --
    -- * We use 'Data.Text.Read.double', which is not as accurate as it
    -- can be;
    -- * Even if we used 'Data.Text.Read.rational' we would not get good
    -- results, because of <https://github.com/bos/text/issues/34>.
    "x: 9.78159610558926e-5" `shouldDecode` object
        [ "x" .= D.Number (read "9.78159610558926e-5") ]

caseTruncatesFiles :: Assertion
caseTruncatesFiles = withSystemTempFile "truncate.yaml" $ \fp h -> do
    replicateM_ 500 $ B8.hPut h "HELLO WORLD!!!!!\n"
    hClose h
    let val = object ["hello" .= ("world" :: String)]
    D.encodeFile fp val
    res <- D.decodeFileEither fp
    either (Left . show) Right res `shouldBe` Right val

caseSpecialKeys :: (HashMap Text () -> B8.ByteString) -> Assertion
caseSpecialKeys encoder = do
      let keys = T.words "true false NO YES 1.2 1e5 null"
          bs = encoder $ M.fromList $ map (, ()) keys
          text = decodeUtf8 bs
      forM_ keys $ \key -> do
        let quoted = T.concat ["'", key, "'"]
        unless (quoted `T.isInfixOf` text) $ error $ concat
          [ "Could not find quoted key: "
          , T.unpack quoted
          , "\n\n"
          , T.unpack text
          ] :: IO ()

taggedSequence :: [Y.Event]
taggedSequence =
  [ Y.EventSequenceStart (Y.UriTag "!foo") Y.FlowSequence Nothing
  , Y.EventSequenceEnd
  ]

taggedMapping :: [Y.Event]
taggedMapping =
  [ Y.EventMappingStart (Y.UriTag "!foo") Y.FlowMapping Nothing
  , Y.EventMappingEnd
  ]

defaultTaggedSequence :: [Y.Event]
defaultTaggedSequence =
  [Y.EventSequenceStart Y.SeqTag Y.FlowSequence Nothing, Y.EventSequenceEnd]

defaultTaggedMapping :: [Y.Event]
defaultTaggedMapping =
  [Y.EventMappingStart Y.MapTag Y.FlowMapping Nothing, Y.EventMappingEnd]

untaggedSequence :: [Y.Event]
untaggedSequence =
  [Y.EventSequenceStart Y.NoTag Y.FlowSequence Nothing, Y.EventSequenceEnd]

untaggedMapping :: [Y.Event]
untaggedMapping =
  [Y.EventMappingStart Y.NoTag Y.FlowMapping Nothing, Y.EventMappingEnd]

mixedTagSampleA :: [Y.Event]
mixedTagSampleA =
  [ Y.EventSequenceStart Y.NoTag Y.BlockSequence Nothing
  , Y.EventMappingStart (Y.UriTag "!foo") Y.FlowMapping Nothing
  , Y.EventMappingEnd
  , Y.EventSequenceEnd
  ]

mixedTagSampleB :: [Y.Event]
mixedTagSampleB =
  [ Y.EventMappingStart Y.NoTag Y.BlockMapping (Just "a")
  , Y.EventScalar "foo" (Y.UriTag "bar") Y.Plain (Just "b")
  , Y.EventSequenceStart (Y.UriTag "!baz") Y.FlowSequence (Just "c")
  , Y.EventScalar "" Y.NullTag Y.Plain (Just "d")
  , Y.EventSequenceEnd
  , Y.EventMappingEnd
  ]
