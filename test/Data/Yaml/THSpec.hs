{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Yaml.THSpec (spec) where

import           Test.Hspec
import           Data.Aeson

import           Data.Yaml.TH

spec :: Spec
spec = do
  describe "yamlQQ" $ do
    it "parses yaml" $ do
      [yamlQQ|
      name: John Doe
      age: 23
      |] `shouldBe` object ["name" .=  String "John Doe", "age" .= Number 23]
