{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
import Test.Framework (defaultMain)

import Text.Libyaml
import qualified Data.ByteString.Char8 as B8
#if MIN_VERSION_transformers(0,2,0)
import "transformers" Control.Monad.Trans.Class
#else
import "transformers" Control.Monad.Trans
#endif

import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, path)

import Control.Exception
import Data.Iteratee hiding (filter, length, foldl')
import Data.List (foldl')

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
    --, testCase "interleaved encode/decode" caseInterleave
    , testCase "decode invalid document (without segfault)" caseDecodeInvalidDocument
    ]

counter :: (Event -> Bool) -> Int -> IterateeG [] Event IO Int
counter pred' acc =
    IterateeG go
  where
    go (EOF x) = return $ Done acc $ EOF x
    go (Chunk c) =
        let acc' = length (filter pred' c) + acc
         in return $ Cont (counter pred' acc') Nothing

caseHelper :: String
           -> (Event -> Bool)
           -> Int
           -> Assertion
caseHelper yamlString pred' expRes = do
    res <- decode (B8.pack yamlString) (counter pred' 0) >>= run
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
    res <- decode yamlBS (counter' accum) >>= run
    res @?= (7, 1, 2)
    where
        yamlString = "foo:\n  baz: [bin1, bin2, bin3]\nbaz: bazval"
        yamlBS = B8.pack yamlString
        counter' acc = IterateeG $ \s ->
            case s of
                EOF x -> return $ Done acc $ EOF x
                Chunk c ->
                    let acc' = foldl' adder acc c
                     in return $ Cont (counter' acc') Nothing
        adder (s, l, m) (EventScalar{})        = (s + 1, l, m)
        adder (s, l, m) (EventSequenceStart{}) = (s, l + 1, m)
        adder (s, l, m) (EventMappingStart{})  = (s, l, m + 1)
        adder a         _                      = a
        accum = (0, 0, 0) :: (Int, Int, Int)

caseLargestString :: Assertion
caseLargestString = do
    res <- decodeFile filePath (dec accum) >>= run
    res @?= (length expected, expected)
    where
        expected = "this one is just a little bit bigger than the others"
        filePath = "test/largest-string.yaml"
        dec acc = IterateeG $ \s ->
            case s of
                EOF x -> return $ Done acc $ EOF x
                Chunk c ->
                    let acc' = foldl' adder acc c
                     in return $ Cont (dec acc') Nothing
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
    eList <- decode yamlBS (dec []) >>= run
    bs <- encode $ mapM_ emitEvent eList
    eList2 <- decode bs (dec []) >>= run
    map MyEvent eList @=? map MyEvent eList2
    where
        yamlString = "foo: bar\nbaz:\n - bin1\n - bin2\n"
        yamlBS = B8.pack yamlString
        dec front = IterateeG $ \s ->
            case s of
                EOF x -> return $ Done front $ EOF x
                Chunk c -> return $ Cont (dec $ front ++ c) Nothing

caseEncodeDecodeFile :: Assertion
caseEncodeDecodeFile = do
    eList <- decodeFile filePath (dec []) >>= run
    encodeFile tmpPath $ mapM_ emitEvent eList
    eList2 <- decodeFile filePath (dec []) >>= run
    map MyEvent eList @=? map MyEvent eList2
    where
        filePath = "test/largest-string.yaml"
        tmpPath = "tmp.yaml"
        dec front = IterateeG $ \s ->
            case s of
                EOF x -> return $ Done front $ EOF x
                Chunk c -> return $ Cont (dec $ front ++ c) Nothing

{-
caseInterleave :: Assertion
caseInterleave = do
    decodeFile filePath $ encodeFile tmpPath inside
    decodeFile tmpPath $ encodeFile tmpPath2 inside
    f1 <- readFile tmpPath
    f2 <- readFile tmpPath2
    f1 @=? f2
    where
        filePath = "test/largest-string.yaml"
        tmpPath = "tmp.yaml"
        tmpPath2 = "tmp2.yaml"
        inside :: YamlEncoder (YamlDecoder IO) ()
        inside = do
            e <- lift parseEvent
            case e of
                EventNone -> return ()
                _ -> emitEvent e >> inside
-}

caseDecodeInvalidDocument :: Assertion
caseDecodeInvalidDocument = do
    x <- decode yamlBS ignore
    y <- runIter x $ EOF Nothing
    case y of
        Cont _ _ -> return ()
        _ -> do
            putStrLn $ "bad return value: " ++ show y
            assertFailure "expected parsing exception, but got no errors"
  where
    yamlString = "  - foo\n  - baz\nbuz"
    yamlBS = B8.pack yamlString
    ignore :: IterateeG [] Event IO ()
    ignore = IterateeG $ \s ->
        case s of
            EOF x -> return $ Done () $ EOF x
            Chunk _c -> return $ Cont ignore Nothing
