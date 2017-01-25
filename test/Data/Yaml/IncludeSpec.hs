{-# LANGUAGE QuasiQuotes #-}
module Data.Yaml.IncludeSpec (main, spec) where

import           Test.Hspec
import           Data.List (isPrefixOf)
import           Data.Aeson
import           Data.Aeson.QQ
import           Data.Yaml (ParseException(InvalidYaml))
import           Data.Yaml.Include
import           Text.Libyaml (YamlException(YamlException))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "decodeFile" $ do
    it "supports includes" $ do
      decodeFile "test/resources/foo.yaml" `shouldReturn` Just [aesonQQ|
        {
          foo: 23,
          bar: {one: 1, two: 2},
          baz: 42
        }
      |]

    it "supports recursive includes" $ do
      decodeFile "test/resources/baz.yaml" `shouldReturn` Just [aesonQQ|
      {
        foo: {
          foo: 23,
          bar: {one: 1, two: 2},
          baz: 42
        }
      }
      |]

    it "aborts on cyclic includes" $ do
      (decodeFile "test/resources/loop/foo.yaml" :: IO (Maybe Value)) `shouldThrow` anyException

    context "when file does not exist" $ do
      it "throws Left (InvalidYaml (Just (YamlException \"Yaml file not found: ...\")))" $ do
        (decodeFile "./does_not_exist.yaml" :: IO (Maybe Value)) `shouldThrow` isYamlFileNotFoundException

  describe "decodeFileEither" $ do
    context "when file does not exist" $ do
      it "returns Left (InvalidYaml (Just (YamlException \"Yaml file not found: ...\")))" $ do
        (decodeFileEither "./does_not_exist.yaml" :: IO (Either ParseException Value)) >>=
          (`shouldSatisfy` either isYamlFileNotFoundException (const False))

isYamlFileNotFoundException :: ParseException -> Bool
isYamlFileNotFoundException (InvalidYaml (Just (YamlException msg)))
  | "Yaml file not found: " `isPrefixOf` msg = True
isYamlFileNotFoundException _ = False
