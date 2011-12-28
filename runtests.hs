{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Framework (defaultMain)

import Text.Libyaml
import qualified Data.ByteString.Char8 as B8

import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, path)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.List (foldl')

import System.Directory
import Control.Monad
import Control.Exception (try, SomeException)

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

counter :: (Event -> Bool) -> Int -> C.Sink Event IO Int
counter pred' acc =
    CL.fold (\cnt e -> (if pred' e then 1 else 0) + cnt) 0

caseHelper :: String
           -> (Event -> Bool)
           -> Int
           -> Assertion
caseHelper yamlString pred' expRes = do
    res <- C.runResourceT $ decode (B8.pack yamlString) C.$$ counter pred' 0
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
    res <- C.runResourceT $ decode yamlBS C.$$ CL.fold adder accum
    res @?= (7, 1, 2)
  where
    yamlString = "foo:\n  baz: [bin1, bin2, bin3]\nbaz: bazval"
    yamlBS = B8.pack yamlString
    adder (s, l, m) (EventScalar{})        = (s + 1, l, m)
    adder (s, l, m) (EventSequenceStart{}) = (s, l + 1, m)
    adder (s, l, m) (EventMappingStart{})  = (s, l, m + 1)
    adder a         _                      = a
    accum = (0, 0, 0) :: (Int, Int, Int)

caseLargestString :: Assertion
caseLargestString = do
    res <- C.runResourceT $ decodeFile filePath C.$$ CL.fold adder accum
    res @?= (length expected, expected)
    where
        expected = "this one is just a little bit bigger than the others"
        filePath = "test/largest-string.yaml"
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
    eList <- C.runResourceT $ decode yamlBS C.$$ CL.consume
    bs <- C.runResourceT $ CL.sourceList eList C.$$ encode
    eList2 <- C.runResourceT $ decode bs C.$$ CL.consume
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
    eList <- C.runResourceT $ decodeFile filePath C.$$ CL.consume
    C.runResourceT $ CL.sourceList eList C.$$ encodeFile tmpPath
    eList2 <- C.runResourceT $ decodeFile filePath C.$$ CL.consume
    map MyEvent eList @=? map MyEvent eList2
  where
    filePath = "test/largest-string.yaml"
    tmpPath = "tmp.yaml"

caseInterleave :: Assertion
caseInterleave = do
    removeFile' tmpPath
    removeFile' tmpPath2
    () <- C.runResourceT $ decodeFile filePath C.$$ encodeFile tmpPath
    () <- C.runResourceT $ decodeFile tmpPath C.$$ encodeFile tmpPath2
    f1 <- readFile tmpPath
    f2 <- readFile tmpPath2
    f1 @=? f2
  where
    filePath = "test/largest-string.yaml"
    tmpPath = "tmp.yaml"
    tmpPath2 = "tmp2.yaml"

caseDecodeInvalidDocument :: Assertion
caseDecodeInvalidDocument = do
    x <- try $ C.runResourceT $ decode yamlBS C.$$ CL.sinkNull
    case x of
        Left (e :: SomeException) -> return ()
        Right y -> do
            putStrLn $ "bad return value: " ++ show y
            assertFailure "expected parsing exception, but got no errors"
  where
    yamlString = "  - foo\n  - baz\nbuz"
    yamlBS = B8.pack yamlString
