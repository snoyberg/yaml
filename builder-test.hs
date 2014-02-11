{-# LANGUAGE OverloadedStrings #-}
import Data.Yaml.Builder
import qualified Data.ByteString as S
import Data.Text (Text)

data Person = Person
    { name :: !Text
    , age :: !Int
    }
instance ToYaml Person where
    toYaml (Person n a) = mapping
        [ "name" .= n
        , "age"  .= a
        ]

main :: IO ()
main = do
    S.putStr $ toByteString
        [ Person "Alice" 57
        , Person "Bob" 23
        , Person "12345" 23
        ]
    writeYamlFile "/tmp/foo.yaml" $
        [ Person "Alice" 57
        , Person "Bob" 23
        , Person "12345" 23
        ]
