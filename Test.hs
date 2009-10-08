import Test.Framework (defaultMain)

import qualified Text.Yaml

main :: IO ()
main = defaultMain
    [ Text.Yaml.testSuite
    ]
