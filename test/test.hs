import Text.Yaml
import qualified Data.ByteString as B
import Control.Monad

main = do
    contents <- B.readFile "input.yaml"
    o <- decode contents
    let contents2 = encode (o :: Object)
    B.writeFile "output.yaml" contents2
    o2 <- decode contents2
    print (o2 :: Object)
    contents3 <- B.readFile "hebrew.yaml"
    o3 <- decode contents3
    print (o3 :: B.ByteString)
    B.writeFile "hebrew.txt" o3

    o4 <- join $ decodeFile "input.yaml"
    print (o4 :: Object)
    encodeFile "output2.yaml" o4
