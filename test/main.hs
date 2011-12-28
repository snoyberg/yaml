{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Text.Libyaml as Y
import qualified Data.ByteString.Char8 as B8

import Test.HUnit hiding (Test, path)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import System.Directory
import Control.Monad
import Control.Exception (try, SomeException)
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()

import Data.Object
import qualified Data.Yaml as D
import Data.Maybe
import Data.Convertible.Text (cs)

main :: IO ()
main = hspecX $ do
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
        it "encode/decode strings" caseEncodeDecodeStrings
        it "decode invalid file" caseDecodeInvalid
        it "encode/decode in order" caseInOrder
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

counter :: (Y.Event -> Bool) -> C.Sink Y.Event IO Int
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

removeFile' :: FilePath -> IO ()
removeFile' fp = do
    x <- doesFileExist fp
    when x $ removeFile fp

caseEncodeDecodeFile :: Assertion
caseEncodeDecodeFile = do
    removeFile' tmpPath
    eList <- C.runResourceT $ Y.decodeFile filePath C.$$ CL.consume
    C.runResourceT $ CL.sourceList eList C.$$ Y.encodeFile tmpPath
    eList2 <- C.runResourceT $ Y.decodeFile filePath C.$$ CL.consume
    map MyEvent eList @=? map MyEvent eList2
  where
    filePath = "test/largest-string.yaml"
    tmpPath = "tmp.yaml"

caseInterleave :: Assertion
caseInterleave = do
    removeFile' tmpPath
    removeFile' tmpPath2
    () <- C.runResourceT $ Y.decodeFile filePath C.$$ Y.encodeFile tmpPath
    () <- C.runResourceT $ Y.decodeFile tmpPath C.$$ Y.encodeFile tmpPath2
    f1 <- readFile tmpPath
    f2 <- readFile tmpPath2
    f1 @=? f2
  where
    filePath = "test/largest-string.yaml"
    tmpPath = "tmp.yaml"
    tmpPath2 = "tmp2.yaml"

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

mkFoldedScalar :: String -> D.YamlScalar
mkFoldedScalar s = D.YamlScalar (cs s) Y.StrTag Y.Folded

mkScalar :: String -> D.YamlScalar
mkScalar s = D.YamlScalar (cs s) Y.NoTag Y.Plain

mkStrScalar :: String -> D.YamlScalar
mkStrScalar s = D.YamlScalar (cs s) Y.StrTag Y.Plain

mappingKey :: D.YamlObject -> String -> D.YamlObject
mappingKey (Mapping m) k = (fromJust . lookup (mkScalar k) $ m)
mappingKey _ _ = error "expected Mapping"

decodeYaml :: String -> Maybe D.YamlObject
decodeYaml s = D.decode $ B8.pack s

sample :: D.YamlObject
sample = Sequence
    [ Scalar $ mkFoldedScalar "foo"
    , Mapping
        [ (mkFoldedScalar "bar1", Scalar $ mkFoldedScalar "bar2")
        ]
    ]

sampleStr :: Object String String
sampleStr = mapKeysValues D.fromYamlScalar D.fromYamlScalar sample

caseEncodeDecodeData :: Assertion
caseEncodeDecodeData = do
    out <- D.decode $ D.encode sample
    out @?= sample

caseEncodeDecodeFileData :: Assertion
caseEncodeDecodeFileData = do
    let fp = "tmp.yaml"
    D.encodeFile fp sample
    out <- join $ D.decodeFile fp
    out @?= sample

caseEncodeDecodeStrings :: Assertion
caseEncodeDecodeStrings = do
    out <- D.decode $ D.encode $ D.toYamlObject sampleStr
    D.fromYamlObject out @?= sampleStr

caseDecodeInvalid :: Assertion
caseDecodeInvalid = do
    let invalid = B8.pack "\tthis is 'not' valid :-)"
    Nothing @=? (D.decode invalid :: Maybe D.YamlObject)

caseSimpleScalarAlias :: Assertion
caseSimpleScalarAlias = do
    let maybeRes = decodeYaml "- &anch foo\n- baz\n- *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Sequence [Scalar (mkScalar "foo"), Scalar (mkScalar "baz"), Scalar (mkScalar "foo")]

caseSimpleSequenceAlias :: Assertion
caseSimpleSequenceAlias = do
    let maybeRes = decodeYaml "seq: &anch\n  - foo\n  - baz\nseq2: *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Mapping [(mkScalar "seq", Sequence [Scalar (mkScalar "foo"), Scalar (mkScalar "baz")]), (mkScalar "seq2", Sequence [Scalar (mkScalar "foo"), Scalar (mkScalar "baz")])]

caseSimpleMappingAlias :: Assertion
caseSimpleMappingAlias = do
    let maybeRes = decodeYaml "map: &anch\n  key1: foo\n  key2: baz\nmap2: *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Mapping [(mkScalar "map", Mapping [(mkScalar "key1", Scalar (mkScalar "foo")), (mkScalar "key2", Scalar (mkScalar "baz"))]), (mkScalar "map2", Mapping [(mkScalar "key1", Scalar (mkScalar "foo")), (mkScalar "key2", Scalar (mkScalar "baz"))])]

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
    res @?= Sequence [Scalar (mkScalar "foo"), Scalar (mkScalar "baz"), Scalar (mkScalar "foo"), Scalar (mkScalar "boo"), Scalar (mkScalar "buz"), Scalar (mkScalar "boo")]

caseAllKeysShouldBeUnique :: Assertion
caseAllKeysShouldBeUnique = do
    let maybeRes = decodeYaml "foo1: foo\nfoo2: baz\nfoo1: buz"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    mappingKey res "foo1" @?= Scalar (mkScalar "buz")

caseSimpleMappingMerge :: Assertion
caseSimpleMappingMerge = do
    let maybeRes = decodeYaml "foo1: foo\nfoo2: baz\n<<:\n  foo1: buz\n  foo3: fuz"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    mappingKey res "foo1" @?= Scalar (mkScalar "foo")
    mappingKey res "foo3" @?= Scalar (mkScalar "fuz")

caseMergeSequence :: Assertion
caseMergeSequence = do
    let maybeRes = decodeYaml "m1: &m1\n  k1: !!str 1\n  k2: !!str 2\nm2: &m2\n  k1: !!str 3\n  k3: !!str 4\nfoo1: foo\n<<: [ *m1, *m2 ]"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    mappingKey res "foo1" @?= Scalar (mkScalar "foo")
    mappingKey res "k1" @?= Scalar (mkStrScalar "1")
    mappingKey res "k2" @?= Scalar (mkStrScalar "2")
    mappingKey res "k3" @?= Scalar (mkStrScalar "4")

inOrderData :: String
inOrderData = "'Fatal': 'Unknown variable \"bar\"'\n'Date': '2001-11-23 15:03:17 -5'\n'User': 'ed'\n'Stack':\n- 'line': '23'\n  'file': 'TopClass.py'\n  'code': 'x = MoreObject(\"345\\n\")\n\n'\n- 'line': '58'\n  'file': 'MoreClass.py'\n  'code': 'foo = bar'\n"

inOrderData2 :: String
inOrderData2 =
       "'a': '1'\n'b': '2'\n'd': '4'\n'c': '3'\n"
    ++ "'g': '1'\n'n': '2'\n'q': '4'\n'f': '3'\n"
    ++ "'z': '1'\n'y': '2'\n'x': '4'\n'w': '3'\n"

caseInOrder :: Assertion
caseInOrder = do
    Just (Mapping ((x, _):_)) <- return $ decodeYaml inOrderData
    x @?= mkScalar "Fatal"
    fmap (B8.unpack . D.encode) (decodeYaml inOrderData2) @?= Just inOrderData2
