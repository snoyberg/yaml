{-# LANGUAGE ScopedTypeVariables #-}
import Data.Yaml (decodeFileEither)
import System.Environment (getArgs)
import System.Exit
import Data.Aeson (encode, Value)
import Prelude hiding (putStr)
import Data.ByteString.Lazy (putStr)

main = do
    args <- getArgs
    case args of
       [] -> print "yaml2json FILE" >> exitFailure
       (f:args) -> do
          ejson <- decodeFileEither f
          case ejson of
             Left err -> print err >> exitFailure
             Right (res :: Value) -> putStr (encode res) >> exitSuccess
       
