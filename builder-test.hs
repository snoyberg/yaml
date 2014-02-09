{-# LANGUAGE OverloadedStrings #-}
import Data.Yaml.Builder
import qualified Data.ByteString as S
import Data.Text (Text)

asText :: Text -> Text
asText = id

asInt :: Int -> Int
asInt = id

main :: IO ()
main = do
    let yb = array
            [ mapping
                [ "name" .= asText "Alice"
                , "age"  .= asInt 57
                ]
            , mapping
                [ "name" .= asText "Bob"
                , "age"  .= asInt 23
                ]
            ]
    S.putStr $ toByteString yb
