module Main where

import qualified Config
import qualified Simple

main :: IO ()
main = do
  Simple.main
  Config.main

