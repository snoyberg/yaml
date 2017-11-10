import Prelude hiding (putStr, getContents)

import Data.Aeson (encode, Value)
import Data.ByteString (getContents)
import Data.ByteString.Lazy (putStr)
import System.Environment (getArgs)
import System.Exit
import System.IO (stderr, hPutStrLn)

import Data.Yaml (decodeFileEither, decodeEither')

helpMessage :: IO ()
helpMessage = putStrLn "Usage: yaml2json FILE\n\nuse '-' as FILE to indicate stdin" >> exitFailure

showJSON :: Show a => Either a Value -> IO b
showJSON ejson =
    case ejson of
       Left err -> hPutStrLn stderr (show err) >> exitFailure
       Right res -> putStr (encode (res :: Value)) >> exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of
       -- strict getContents will read in all of stdin at once
       (["-h"]) -> helpMessage
       (["--help"]) -> helpMessage
       (["-"]) -> getContents >>= showJSON . decodeEither'
       ([f])   -> decodeFileEither f >>= showJSON
       _ -> helpMessage

