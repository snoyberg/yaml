import Text.Yaml
import qualified Data.ByteString as B

main = do
    bs <- B.readFile "tags.yaml"
    yo <- decodeYaml' bs
    print yo
