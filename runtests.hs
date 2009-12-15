import Test.Framework (defaultMain)

import qualified Data.Object.Yaml

main :: IO ()
main = defaultMain
    [ Data.Object.Yaml.testSuite
    ]
