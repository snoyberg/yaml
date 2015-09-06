import Control.Monad (when)
import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import System.Environment (getArgs)

import qualified Data.Yaml as Y

main :: IO ()
main = do
    args <- getArgs
    when (length args > 2) $ error "Usage: json2yaml [in] [out]"
    let (input:output:_) = args ++ repeat "-"
    mval <- fmap J.decode $
        case input of
            "-" -> L.getContents
            _ -> L.readFile input
    case mval of
        Nothing -> error "Invalid input JSON"
        Just val -> case output of
            "-" -> S.putStr $ Y.encode (val :: Y.Value)
            _ -> Y.encodeFile output val
