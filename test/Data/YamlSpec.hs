{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.YamlSpec (main, spec) where

import qualified Text.Libyaml as Y
import qualified Data.ByteString.Char8 as B8

import Test.HUnit hiding (Test, path)

import qualified Data.Conduit as C
import qualified Control.Monad.Trans.Resource as C
import qualified Data.Conduit.List as CL

import Control.Monad
import Control.Exception (try, SomeException)
import Test.Hspec
import Data.Either.Compat
import System.Directory (createDirectory, createDirectoryIfMissing)
import Test.Mockery.Directory

import qualified Data.Yaml as D
import qualified Data.Yaml.Pretty as Pretty
import Data.Yaml (object, array, (.=))
import Data.Maybe
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Data.Aeson.TH
import Data.Text (Text)
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

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "streaming" $ do
        it "count scalars with anchor" caseCountScalarsWithAnchor
        it "count sequences with anchor" caseCountSequencesWithAnchor
        it "count mappings with anchor" caseCountMappingsWithAnchor
        it "count aliases" caseCountAliases
        it "count scalars" caseCountScalars
        it "largest string" caseLargestString
        it "encode/decode" caseEncodeDecode
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
    describe "Data.Yaml.Pretty" $ do
        it "encode/decode" caseEncodeDecodeDataPretty
        it "encode/decode strings" caseEncodeDecodeStringsPretty
        it "processes datatypes" caseDataTypesPretty
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

    describe "special keys" $ do
        let tester key = it (T.unpack key) $
                let value = object [key .= True]
                 in D.decode (D.encode value) `shouldBe` Just value
        mapM_ tester specialStrings

    describe "special values" $ do
        let tester value = it (T.unpack value) $
                let value' = object ["foo" .= value]
                 in D.decode (D.encode value') `shouldBe` Just value'
        mapM_ tester specialStrings

    describe "decodeFileEither" $ do
        it "loads YAML through JSON into Haskell data" $ do
          tj <- either (error . show) id `fmap` D.decodeFileEither "test/json.yaml"
          tj `shouldBe` TestJSON
                          { string = "str"
                          , number = 2
                          , anArray = V.fromList ["a", "b"]
                          , hash = HM.fromList [("key1", "value1"), ("key2", "value2")]
                          , extrastring = "1234-foo"
                          }

        context "when file does not exist" $ do
            it "returns Left" $ do
                (D.decodeFileEither "./does_not_exist.yaml" :: IO (Either D.ParseException D.Value)) >>= (`shouldSatisfy` isLeft)


    describe "round-tripping of special scalars" $ do
        let special = words "y Y On ON false 12345 12345.0 12345a 12e3"
        forM_ special $ \w -> it w $
            let v = object ["word" .= w]
             in D.decode (D.encode v) `shouldBe` Just v
        it "no tags" $ D.encode (object ["word" .= ("true" :: String)]) `shouldBe` "word: 'true'\n"

    it "aliases in keys #49" caseIssue49

    it "serialization of +123 #64" $ do
        D.decode (D.encode ("+123" :: String)) `shouldBe` Just ("+123" :: String)

    it "preserves Scientific precision" casePreservesScientificPrecision

    it "truncates files" caseTruncatesFiles


specialStrings :: [T.Text]
specialStrings =
    [ "fo\"o"
    , "fo\'o"
    , "fo\\'o"
    , "fo: o"
    , "foo\nbar\nbaz\n"
    ]

counter :: Monad m => (Y.Event -> Bool) -> C.Sink Y.Event m Int
counter pred' =
    CL.fold (\cnt e -> (if pred' e then 1 else 0) + cnt) 0

caseHelper :: String
           -> (Y.Event -> Bool)
           -> Int
           -> Assertion
caseHelper yamlString pred' expRes = do
    res <- C.runResourceT $ Y.decode (B8.pack yamlString) C.$$ counter pred'
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
    isSequenceStartA (Y.EventSequenceStart (Just _)) = True
    isSequenceStartA _ = False

caseCountMappingsWithAnchor :: Assertion
caseCountMappingsWithAnchor =
    caseHelper yamlString isMappingA 1
  where
    yamlString = "foo: &anchor\n  key1: bin1\n  key2: bin2\n  key3: bin3"
    isMappingA (Y.EventMappingStart (Just _)) = True
    isMappingA _ = False

caseCountAliases :: Assertion
caseCountAliases =
    caseHelper yamlString isAlias 1
  where
    yamlString = "foo: &anchor\n  key1: bin1\n  key2: bin2\n  key3: bin3\nboo: *anchor"
    isAlias Y.EventAlias{} = True
    isAlias _ = False

caseCountScalars :: Assertion
caseCountScalars = do
    res <- C.runResourceT $ Y.decode yamlBS C.$$ CL.fold adder accum
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
    res <- C.runResourceT $ Y.decodeFile filePath C.$$ CL.fold adder accum
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
    eList <- C.runResourceT $ Y.decode yamlBS C.$$ CL.consume
    bs <- C.runResourceT $ CL.sourceList eList C.$$ Y.encode
    eList2 <- C.runResourceT $ Y.decode bs C.$$ CL.consume
    map MyEvent eList @=? map MyEvent eList2
  where
    yamlString = "foo: bar\nbaz:\n - bin1\n - bin2\n"
    yamlBS = B8.pack yamlString

caseEncodeDecodeFile :: Assertion
caseEncodeDecodeFile = withFile "" $ \tmpPath -> do
    eList <- C.runResourceT $ Y.decodeFile filePath C.$$ CL.consume
    C.runResourceT $ CL.sourceList eList C.$$ Y.encodeFile tmpPath
    eList2 <- C.runResourceT $ Y.decodeFile filePath C.$$ CL.consume
    map MyEvent eList @=? map MyEvent eList2
  where
    filePath = "test/largest-string.yaml"

caseInterleave :: Assertion
caseInterleave = withFile "" $ \tmpPath -> withFile "" $ \tmpPath2 -> do
    () <- C.runResourceT $ Y.decodeFile filePath C.$$ Y.encodeFile tmpPath
    () <- C.runResourceT $ Y.decodeFile tmpPath C.$$ Y.encodeFile tmpPath2
    f1 <- readFile tmpPath
    f2 <- readFile tmpPath2
    f1 @=? f2
  where
    filePath = "test/largest-string.yaml"

caseDecodeInvalidDocument :: Assertion
caseDecodeInvalidDocument = do
    x <- try $ C.runResourceT $ Y.decode yamlBS C.$$ CL.sinkNull
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

decodeYaml :: String -> Maybe D.Value
decodeYaml s = D.decode $ B8.pack s

sample :: D.Value
sample = array
    [ D.String "foo"
    , object
        [ ("bar1", D.String "bar2")
        , ("bar3", D.String "")
        ]
    , D.String ""
    ]

caseEncodeDecodeData :: Assertion
caseEncodeDecodeData = do
    let out = D.decode $ D.encode sample
    out @?= Just sample

caseEncodeDecodeDataPretty :: Assertion
caseEncodeDecodeDataPretty = do
    let out = D.decode $ Pretty.encodePretty Pretty.defConfig sample
    out @?= Just sample

caseEncodeDecodeFileData :: Assertion
caseEncodeDecodeFileData = withFile "" $ \fp -> do
    D.encodeFile fp sample
    out <- D.decodeFile fp
    out @?= Just sample

caseEncodeDecodeNonAsciiFileData :: Assertion
caseEncodeDecodeNonAsciiFileData = do
  let mySample = (object ["foo" .= True])
  inTempDirectory $ do
    createDirectory "accenté"
    D.encodeFile "accenté/bar.yaml" mySample
    out1 <- D.decodeFile "accenté/bar.yaml"
    out1 @?= Just mySample

  createDirectoryIfMissing True "test/resources/accenté/"

  readFile "test/resources/accent/foo.yaml" >>=
    writeFile "test/resources/accenté/foo.yaml"
  out2 <- D.decodeFile "test/resources/accenté/foo.yaml"
  out2 @?= Just mySample

caseEncodeDecodeStrings :: Assertion
caseEncodeDecodeStrings = do
    let out = D.decode $ D.encode sample
    out @?= Just sample

caseEncodeDecodeStringsPretty :: Assertion
caseEncodeDecodeStringsPretty = do
    let out = D.decode $ Pretty.encodePretty Pretty.defConfig sample
    out @?= Just sample

caseDecodeInvalid :: Assertion
caseDecodeInvalid = do
    let invalid = B8.pack "\tthis is 'not' valid :-)"
    Nothing @=? (D.decode invalid :: Maybe D.Value)

caseSimpleScalarAlias :: Assertion
caseSimpleScalarAlias = do
    let maybeRes = decodeYaml "- &anch foo\n- baz\n- *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= array [(mkScalar "foo"), (mkScalar "baz"), (mkScalar "foo")]

caseSimpleSequenceAlias :: Assertion
caseSimpleSequenceAlias = do
    let maybeRes = decodeYaml "seq: &anch\n  - foo\n  - baz\nseq2: *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= object [("seq", array [(mkScalar "foo"), (mkScalar "baz")]), ("seq2", array [(mkScalar "foo"), (mkScalar "baz")])]

caseSimpleMappingAlias :: Assertion
caseSimpleMappingAlias = do
    let maybeRes = decodeYaml "map: &anch\n  key1: foo\n  key2: baz\nmap2: *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= object [(T.pack "map", object [("key1", mkScalar "foo"), ("key2", (mkScalar "baz"))]), (T.pack "map2", object [("key1", (mkScalar "foo")), ("key2", mkScalar "baz")])]

caseMappingAliasBeforeAnchor :: Assertion
caseMappingAliasBeforeAnchor = do
    let res = decodeYaml "map: *anch\nmap2: &anch\n  key1: foo\n  key2: baz"
    isNothing res @? "decode should return Nothing due to unknown alias"

caseMappingAliasInsideAnchor :: Assertion
caseMappingAliasInsideAnchor = do
    let res = decodeYaml "map: &anch\n  key1: foo\n  key2: *anch"
    isNothing res @? "decode should return Nothing due to unknown alias"

caseScalarAliasOverriding :: Assertion
caseScalarAliasOverriding = do
    let maybeRes = decodeYaml "- &anch foo\n- baz\n- *anch\n- &anch boo\n- buz\n- *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= array [(mkScalar "foo"), (mkScalar "baz"), (mkScalar "foo"), (mkScalar "boo"), (mkScalar "buz"), (mkScalar "boo")]

caseAllKeysShouldBeUnique :: Assertion
caseAllKeysShouldBeUnique = do
    let maybeRes = decodeYaml "foo1: foo\nfoo2: baz\nfoo1: buz"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    mappingKey res "foo1" @?= (mkScalar "buz")

caseSimpleMappingMerge :: Assertion
caseSimpleMappingMerge = do
    let maybeRes = decodeYaml "foo1: foo\nfoo2: baz\n<<:\n  foo1: buz\n  foo3: fuz"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    mappingKey res "foo1" @?= (mkScalar "foo")
    mappingKey res "foo3" @?= (mkScalar "fuz")

caseMergeSequence :: Assertion
caseMergeSequence = do
    let maybeRes = decodeYaml "m1: &m1\n  k1: !!str 1\n  k2: !!str 2\nm2: &m2\n  k1: !!str 3\n  k3: !!str 4\nfoo1: foo\n<<: [ *m1, *m2 ]"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    mappingKey res "foo1" @?= (mkScalar "foo")
    mappingKey res "k1" @?= (D.String "1")
    mappingKey res "k2" @?= (D.String "2")
    mappingKey res "k3" @?= (D.String "4")

caseDataTypes :: Assertion
caseDataTypes =
    D.decode (D.encode val) @?= Just val
  where
    val = object
        [ ("string", D.String "foo")
        , ("int", D.Number 5)
        , ("float", D.Number 4.3)
        , ("true", D.Bool True)
        , ("false", D.Bool False)
        , ("null", D.Null)
        ]

caseDataTypesPretty :: Assertion
caseDataTypesPretty =
    D.decode (Pretty.encodePretty Pretty.defConfig val) @?= Just val
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
caseQuotedNumber = D.decode "foo: \"1234\"" @?= Just (object [("foo", D.String "1234")])
caseUnquotedNumber = D.decode "foo: 1234" @?= Just (object [("foo", D.Number 1234)])
caseAttribNumber = D.decode "foo: !!str 1234" @?= Just (object [("foo", D.String "1234")])
caseIntegerDecimals = D.encode (1 :: Int) @?= "1\n...\n"

obj :: Maybe D.Value
obj = Just (object [("foo", D.Bool False), ("bar", D.Bool True), ("baz", D.Bool True)])

caseLowercaseBool, caseUppercaseBool, caseTitlecaseBool :: Assertion
caseLowercaseBool = D.decode "foo: off\nbar: y\nbaz: true" @?= obj
caseUppercaseBool = D.decode "foo: FALSE\nbar: Y\nbaz: ON" @?= obj
caseTitlecaseBool = D.decode "foo: No\nbar: Yes\nbaz: True" @?= obj

caseEmptyInput :: Assertion
caseEmptyInput = D.decodeEither B8.empty @?= Right D.Null

caseEmptyInputFile :: Assertion
caseEmptyInputFile = do
    out <- D.decodeFileEither "test/resources/empty.yaml"
    either (Left . D.prettyPrintParseException) Right out @?= Right D.Null

caseCommentOnlyInput :: Assertion
caseCommentOnlyInput = D.decodeEither "# comment\n" @?= Right D.Null

caseCommentOnlyInputFile :: Assertion
caseCommentOnlyInputFile = do
    out <- D.decodeFileEither "test/resources/empty2.yaml"
    either (Left . D.prettyPrintParseException) Right out @?= Right D.Null

checkNull :: T.Text -> Spec
checkNull x =
    it ("null recognized: " ++ show x) assertion
  where
    assertion = Just (object [("foo", D.Null)]) @=? D.decode (B8.pack $ "foo: " ++ T.unpack x)

caseStripAlias :: Assertion
caseStripAlias =
    D.decode src @?= Just (object
        [ "Default" .= object
            [ "foo" .= (1 :: Int)
            , "bar" .= (2 :: Int)
            ]
        , "Obj" .= object
            [ "foo" .= (1 :: Int)
            , "bar" .= (2 :: Int)
            , "key" .= (3 :: Int)
            ]
        ])
  where
    src = "Default: &def\n  foo: 1\n  bar: 2\nObj:\n  <<: *def\n  key: 3\n"

caseIssue49 :: Assertion
caseIssue49 =
    D.decodeEither src @?= Right (object
        [ "a" .= object [ "value" .= (1.0 :: Double) ]
        , "b" .= object [ "value" .= (1.2 :: Double) ]
        ])
  where
    src = "---\na:\n  &id5 value: 1.0\nb:\n  *id5: 1.2"

-- | We cannot guarantee this before aeson started using 'Scientific'.
casePreservesScientificPrecision :: Assertion
casePreservesScientificPrecision = do
    D.decodeEither "x: 1e-100000" @?= Right (object
        [ "x" .= D.Number (read "1e-100000") ])
    -- Note that this ought to work also without 'Scientific', given
    -- that @read (show "9.78159610558926e-5") == 9.78159610558926e-5@.
    -- However, it didn't work (and still doesn't work with aeson < 0.7)
    -- for two reasons:
    --
    -- * We use 'Data.Text.Read.double', which is not as accurate as it
    -- can be;
    -- * Even if we used 'Data.Text.Read.rational' we would not get good
    -- results, because of <https://github.com/bos/text/issues/34>.
    D.decodeEither "x: 9.78159610558926e-5" @?= Right (object
        [ "x" .= D.Number (read "9.78159610558926e-5") ])

caseTruncatesFiles :: Assertion
caseTruncatesFiles = withSystemTempFile "truncate.yaml" $ \fp h -> do
    replicateM_ 500 $ B8.hPut h "HELLO WORLD!!!!!\n"
    hClose h
    let val = object ["hello" .= ("world" :: String)]
    D.encodeFile fp val
    res <- D.decodeFileEither fp
    either (Left . show) Right res `shouldBe` Right val
