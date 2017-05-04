module Main where

import qualified Config
import qualified Simple
import qualified Config2

main :: IO ()
main = do
  Simple.main
  Config.main
  Config2.main


