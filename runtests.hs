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
