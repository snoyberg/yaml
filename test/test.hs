{-# LANGUAGE MultiParamTypeClasses #-}
import Text.Yaml
import qualified Data.ByteString as B
import Control.Monad
import Data.Convertible

-- FIXME
instance ConvertSuccess B.ByteString B.ByteString where
    convertSuccess = id
instance ConvertAttempt B.ByteString B.ByteString where
    convertAttempt = return

main = do
    contents <- B.readFile "input.yaml"
    o <- decodeText contents
    let contents2 = encodeText o
    B.writeFile "output.yaml" contents2
    o2 <- decodeText contents2
    print o2
{-
    contents3 <- B.readFile "hebrew.yaml"
    o3 <- decodeText contents3
    print (o3 :: B.ByteString)
    B.writeFile "hebrew.txt" o3

    o4 <- join $ decodeFile "input.yaml"
    print (o4 :: Object)
    encodeFile "output2.yaml" o4
-}
