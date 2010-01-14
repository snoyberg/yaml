import Test.Framework (defaultMain)

import Text.Libyaml
import qualified Data.ByteString.Char8 as B8
import Control.Monad.Trans

--import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit hiding (Test, path)
--import Test.QuickCheck

main :: IO ()
main = defaultMain
    [ testCase "count scalars" caseCountScalars
    , testCase "largest string" caseLargestString
    , testCase "encode/decode" caseEncodeDecode
    , testCase "encode/decode file" caseEncodeDecodeFile
    , testCase "interleaved encode/decode" caseInterleave
    ]

caseCountScalars :: Assertion
caseCountScalars = do
    res <- decode yamlBS $ counter accum
    res @?= (7, 1, 2)
    where
        yamlString = "foo:\n  baz: [bin1, bin2, bin3]\nbaz: bazval"
        yamlBS = B8.pack yamlString
        counter (s, l, m) = do
            e <- parseEvent
            case e of
                EventScalar {} -> counter (s + 1, l, m)
                EventSequenceStart -> counter (s, l + 1, m)
                EventMappingStart -> counter (s, l, m + 1)
                EventNone -> return (s, l, m)
                _ -> counter (s, l, m)
        accum = (0, 0, 0) :: (Int, Int, Int)

caseLargestString :: Assertion
caseLargestString = do
    res <- decodeFile filePath $ dec accum
    res @?= (length expected, expected)
    where
        expected = "this one is just a little bit bigger than the others"
        filePath = "test/largest-string.yaml"
        dec (i, s) = do
            e <- parseEvent
            case e of
                (EventScalar bs _ _) -> do
                    let s' = B8.unpack bs
                        i' = length s'
                    dec $ if i' > i then (i', s') else (i, s)
                EventNone -> return (i, s)
                _ -> dec (i, s)
        accum = (0, "no strings found")

caseEncodeDecode :: Assertion
caseEncodeDecode = do
    eList <- decode yamlBS $ dec id
    bs <- encode $ mapM_ emitEvent eList
    eList2 <- decode bs $ dec id
    map MyEvent eList @=? map MyEvent eList2
    where
        yamlString = "foo: bar\nbaz:\n - bin1\n - bin2\n"
        yamlBS = B8.pack yamlString
        dec front = do
            e <- parseEvent
            case e of
                EventNone -> return $ front []
                _ -> dec $ front . (:) e

caseEncodeDecodeFile :: Assertion
caseEncodeDecodeFile = do
    eList <- decodeFile filePath $ dec id
    encodeFile tmpPath $ mapM_ emitEvent eList
    eList2 <- decodeFile filePath $ dec id
    map MyEvent eList @=? map MyEvent eList2
    where
        filePath = "test/largest-string.yaml"
        tmpPath = "tmp.yaml"
        dec front = do
            e <- parseEvent
            case e of
                EventNone -> return $ front []
                _ -> dec $ front . (:) e

newtype MyEvent = MyEvent Event deriving Show
instance Eq MyEvent where
    (MyEvent (EventScalar s t _)) == (MyEvent (EventScalar s' t' _)) =
        s == s' && t == t'
    MyEvent e1 == MyEvent e2 = e1 == e2

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
