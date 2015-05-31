{-# LANGUAGE QuasiQuotes #-}
module Data.Yaml.IncludeSpec (main, spec) where

import           Test.Hspec
import           Data.Either.Compat
import           Data.Aeson
import           Data.Aeson.QQ
import           Data.Yaml (ParseException)

import           Data.Yaml.Include

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

  describe "decodeFileEither" $ do
    context "when file does not exist" $ do
      it "returns Left" $ do
        (decodeFileEither "./does_not_exist.yaml" :: IO (Either ParseException Value)) >>= (`shouldSatisfy` isLeft)
