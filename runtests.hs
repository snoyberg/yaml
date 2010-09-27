{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
import Test.Framework (defaultMain)

import Text.Libyaml
import qualified Data.ByteString.Char8 as B8

import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, path)

import qualified Data.Enumerator as E
import Data.Enumerator (($$))
import Data.List (foldl')

import System.Directory
import Control.Monad

main :: IO ()
main = defaultMain
    [ testCase "count scalars with anchor" caseCountScalarsWithAnchor
    , testCase "count sequences with anchor" caseCountSequencesWithAnchor
    , testCase "count mappings with anchor" caseCountMappingsWithAnchor
    , testCase "count aliases" caseCountAliases
    , testCase "count scalars" caseCountScalars
    , testCase "largest string" caseLargestString
    , testCase "encode/decode" caseEncodeDecode
    , testCase "encode/decode file" caseEncodeDecodeFile
    , testCase "interleaved encode/decode" caseInterleave
    , testCase "decode invalid document (without segfault)" caseDecodeInvalidDocument
    ]

counter :: (Event -> Bool) -> Int -> E.Step Event IO Int
counter pred' acc =
    E.Continue go
  where
    go E.EOF = E.yield acc E.EOF
    go (E.Chunks c) = E.returnI $ counter pred' $ length (filter pred' c) + acc

caseHelper :: String
           -> (Event -> Bool)
           -> Int
           -> Assertion
caseHelper yamlString pred' expRes = do
    Right res <- E.run $ decode (B8.pack yamlString) (counter pred' 0)
    res @?= expRes

caseCountScalarsWithAnchor :: Assertion
caseCountScalarsWithAnchor =
    caseHelper yamlString isScalarA 1
  where
    yamlString = "foo:\n  - &anchor bin1\n  - bin2\n  - bin3"
    isScalarA (EventScalar _ _ _ (Just _)) = True
    isScalarA _ = False

caseCountSequencesWithAnchor :: Assertion
caseCountSequencesWithAnchor =
    caseHelper yamlString isSequenceStartA 1
  where
    yamlString = "foo: &anchor\n  - bin1\n  - bin2\n  - bin3"
    isSequenceStartA (EventSequenceStart (Just _)) = True
    isSequenceStartA _ = False

caseCountMappingsWithAnchor :: Assertion
caseCountMappingsWithAnchor =
    caseHelper yamlString isMappingA 1
  where
    yamlString = "foo: &anchor\n  key1: bin1\n  key2: bin2\n  key3: bin3"
    isMappingA (EventMappingStart (Just _)) = True
    isMappingA _ = False

caseCountAliases :: Assertion
caseCountAliases =
    caseHelper yamlString isAlias 1
  where
    yamlString = "foo: &anchor\n  key1: bin1\n  key2: bin2\n  key3: bin3\nboo: *anchor"
    isAlias EventAlias{} = True
    isAlias _ = False

caseCountScalars :: Assertion
caseCountScalars = do
    Right res <- E.run $ decode yamlBS $ counter' accum
    res @?= (7, 1, 2)
  where
    yamlString = "foo:\n  baz: [bin1, bin2, bin3]\nbaz: bazval"
    yamlBS = B8.pack yamlString
    counter' acc = E.Continue $ \s ->
        case s of
            E.EOF -> E.yield acc E.EOF
            E.Chunks c -> E.returnI $ counter' $ foldl' adder acc c
    adder (s, l, m) (EventScalar{})        = (s + 1, l, m)
    adder (s, l, m) (EventSequenceStart{}) = (s, l + 1, m)
    adder (s, l, m) (EventMappingStart{})  = (s, l, m + 1)
    adder a         _                      = a
    accum = (0, 0, 0) :: (Int, Int, Int)

caseLargestString :: Assertion
caseLargestString = do
    Right res <- E.run $ decodeFile filePath $ dec accum
    res @?= (length expected, expected)
    where
        expected = "this one is just a little bit bigger than the others"
        filePath = "test/largest-string.yaml"
        dec acc = E.Continue $ \s ->
            case s of
                E.EOF -> E.yield acc E.EOF
                E.Chunks c ->
                    let acc' = foldl' adder acc c
                     in E.returnI $ dec acc'
        adder (i, s) (EventScalar bs _ _ _) =
            let s' = B8.unpack bs
                i' = length s'
             in if i' > i then (i', s') else (i, s)
        adder acc _ = acc
        accum = (0, "no strings found")

newtype MyEvent = MyEvent Event deriving Show
instance Eq MyEvent where
    (MyEvent (EventScalar s t _ _)) == (MyEvent (EventScalar s' t' _ _)) =
        s == s' && t == t'
    MyEvent e1 == MyEvent e2 = e1 == e2

caseEncodeDecode :: Assertion
caseEncodeDecode = do
    Right eList <- E.run $ decode yamlBS $$ E.consume
    Right bs <- E.run $ E.enumList 1 eList $$ encode
    Right eList2 <- E.run $ decode bs $$ E.consume
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
    Right eList <- E.run $ decodeFile filePath $$ E.consume
    E.run $ E.enumList 1 eList $$ encodeFile tmpPath
    Right eList2 <- E.run $ decodeFile filePath $$ E.consume
    map MyEvent eList @=? map MyEvent eList2
  where
    filePath = "test/largest-string.yaml"
    tmpPath = "tmp.yaml"

caseInterleave :: Assertion
caseInterleave = do
    removeFile' tmpPath
    removeFile' tmpPath2
    Right () <- E.run $ decodeFile filePath $$ encodeFile tmpPath
    Right () <- E.run $ decodeFile tmpPath $$ encodeFile tmpPath2
    f1 <- readFile tmpPath
    f2 <- readFile tmpPath2
    f1 @=? f2
  where
    filePath = "test/largest-string.yaml"
    tmpPath = "tmp.yaml"
    tmpPath2 = "tmp2.yaml"

caseDecodeInvalidDocument :: Assertion
caseDecodeInvalidDocument = do
    x <- E.run $ decode yamlBS ignore
    case x of
        Left _ -> return ()
        Right y -> do
            putStrLn $ "bad return value: " ++ show y
            assertFailure "expected parsing exception, but got no errors"
  where
    yamlString = "  - foo\n  - baz\nbuz"
    yamlBS = B8.pack yamlString
    ignore = E.Continue $ \s ->
        case s of
            E.EOF -> E.yield () E.EOF
            E.Chunks _-> E.returnI ignore
