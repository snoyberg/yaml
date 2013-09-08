{-# LANGUAGE ScopedTypeVariables #-}

import Data.Yaml (decodeFileEither, decodeEither')
import System.Environment (getArgs)
import System.Exit
import Data.Aeson (encode, Value)
import Prelude hiding (putStr, getContents)
import Data.ByteString.Lazy (putStr)
import Data.ByteString (getContents)

helpMessage :: IO ()
helpMessage = putStrLn "yaml2json FILE\n  use - as FILE to indicate stdin" >> exitFailure

showJSON ejson =
    case ejson of
       Left err -> print err >> exitFailure
       Right (res :: Value) -> putStr (encode res) >> exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of
       [] -> helpMessage
       (f:a:_)  -> helpMessage
       -- strict getContents will read in all of stdin at once
       ("-":[]) -> getContents >>= showJSON . decodeEither'
       (f:[])   -> decodeFileEither f >>= showJSON
       
