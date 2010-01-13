import Test.Framework (defaultMain)

import Text.Libyaml
import qualified Data.ByteString.Char8 as B8

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
    res <- decode yamlBS fold accum
    res @?= (7, 1, 2)
    where
        yamlString = "foo:\n  baz: [bin1, bin2, bin3]\nbaz: bazval"
        yamlBS = B8.pack yamlString
        fold (s, l, m) (EventScalar {}) = return $ More (s + 1, l, m)
        fold (s, l, m) EventSequenceStart = return $ More (s, l + 1, m)
        fold (s, l, m) EventMappingStart = return $ More (s, l, m + 1)
        fold res EventNone = return $ Done res
        fold res _ = return $ More res
        accum = (0, 0, 0) :: (Int, Int, Int)

caseLargestString :: Assertion
caseLargestString = do
    res <- decodeFile filePath fold accum
    res @?= (length expected, expected)
    where
        expected = "this one is just a little bit bigger than the others"
        filePath = "test/largest-string.yaml"
        fold (i, s) (EventScalar bs _ _) =
            let s' = B8.unpack bs
                i' = length s'
             in if i' > i
                    then return $ More (i', s')
                    else return $ More (i, s)
        fold res EventNone = return $ Done res
        fold res _ = return $ More res
        accum = (0, "no strings found")

caseEncodeDecode :: Assertion
caseEncodeDecode = do
    eList' <- decode yamlBS fold id
    let eList = eList' []
    bs <- encode $ mapM_ emitEvent eList
    eList2' <- decode bs fold id
    let eList2 = eList2' []
    map MyEvent eList @=? map MyEvent eList2
    where
        yamlString = "foo: bar\nbaz:\n - bin1\n - bin2\n"
        yamlBS = B8.pack yamlString
        fold x EventNone = return $ Done x
        fold x e = return $ More $ x . (:) e

caseEncodeDecodeFile :: Assertion
caseEncodeDecodeFile = do
    eList' <- decodeFile filePath fold id
    let eList = eList' []
    encodeFile tmpPath $ mapM_ emitEvent eList
    eList2' <- decodeFile filePath fold id
    let eList2 = eList2' []
    map MyEvent eList @=? map MyEvent eList2
    where
        filePath = "test/largest-string.yaml"
        tmpPath = "tmp.yaml"
        fold x EventNone = return $ Done x
        fold x e = return $ More $ x . (:) e

newtype MyEvent = MyEvent Event deriving Show
instance Eq MyEvent where
    (MyEvent (EventScalar s t _)) == (MyEvent (EventScalar s' t' _)) =
        s == s' && t == t'
    MyEvent e1 == MyEvent e2 = e1 == e2

caseInterleave :: Assertion
caseInterleave = do
    encodeFile tmpPath $ decodeFile filePath emitEvent' ()
    encodeFile tmpPath2 $ decodeFile tmpPath emitEvent' ()
    f1 <- readFile tmpPath
    f2 <- readFile tmpPath2
    f1 @=? f2
    where
        filePath = "test/largest-string.yaml"
        tmpPath = "tmp.yaml"
        tmpPath2 = "tmp2.yaml"
        emitEvent' () EventNone = return $ Done ()
        emitEvent' () e = emitEvent e >> return (More ())
