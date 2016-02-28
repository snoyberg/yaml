{-# LANGUAGE OverloadedStrings #-}

module Simple where

import qualified Data.Yaml as Y

main = do
  print $ (Y.decode "[1,2,3]" :: Maybe [Integer])
