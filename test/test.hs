{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
import Text.Yaml
import qualified Data.ByteString as B
import Control.Monad
import Data.Convertible.Text
import Data.Object
import Data.Object.Text

main = do
    contents <- B.readFile "input.yaml"
    o <- decodeText' contents
    let contents2 = encodeText o
    B.writeFile "output.yaml" contents2
    o2 <- decodeText' contents2
    print o2

    contents3 <- B.readFile "hebrew.yaml"
    o3 <- decodeText contents3
    print o3
    B.writeFile "hebrew.txt" o3

instance ConvertAttempt (Object Text Text) B.ByteString where
    convertAttempt = fromObject
