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
    ]

caseCountScalars :: Assertion
caseCountScalars = do
    Right res <- decode yamlBS fold accum
    res @?= (7, 1, 2)
    where
        yamlString = "foo:\n  baz: [bin1, bin2, bin3]\nbaz: bazval"
        yamlBS = B8.pack yamlString
        fold (s, l, m) (EventScalar {}) = Right (s + 1, l, m)
        fold (s, l, m) EventSequenceStart = Right (s, l + 1, m)
        fold (s, l, m) EventMappingStart = Right (s, l, m + 1)
        fold res EventNone = Left res
        fold res _ = Right res
        accum = (0, 0, 0) :: (Int, Int, Int)

caseLargestString :: Assertion
caseLargestString = do
    Right res <- decodeFile filePath fold accum
    res @?= (length expected, expected)
    where
        expected = "this one is just a little bit bigger than the others"
        filePath = "test/largest-string.yaml"
        fold (i, s) (EventScalar bs _ _) =
            let s' = B8.unpack bs
                i' = length s'
             in if i' > i
                    then Right (i', s')
                    else Right (i, s)
        fold res EventNone = Left res
        fold res _ = Right res
        accum = (0, "no strings found")
