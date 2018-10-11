{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Yaml.IncludeSpec (main, spec) where

import           Test.Hspec
import           Data.List (isPrefixOf)
import qualified Data.ByteString.Lazy as LB
import           Data.Aeson
import           Data.Aeson.Internal (JSONPathElement(..))
import           Data.Yaml (ParseException(InvalidYaml))
import           Data.Yaml.Include
import           Data.Yaml.Internal
import           Text.Libyaml (YamlException(YamlException))
import           Test.Mockery.Directory
import           Text.RawString.QQ
import           Data.Yaml.TH (yamlQQ)

main :: IO ()
main = hspec spec

asInt :: Int -> Int
asInt = id

spec :: Spec
spec = do
  describe "decodeFile" $ do
    it "supports includes" $ do
      decodeFile "test/resources/foo.yaml" `shouldReturn` Just (object
          [ "foo" .= asInt 23
          , "bar" .= object
              [ "one" .= asInt 1
              , "two" .= asInt 2
              ]
          , "baz" .= asInt 42
          ])

    it "supports recursive includes" $ do
      decodeFile "test/resources/baz.yaml" `shouldReturn` Just (object
        [ "foo" .= object
          [ "foo" .= asInt 23
          , "bar" .= object
            [ "one" .= asInt 1
            , "two" .= asInt 2
            ]
          , "baz" .= asInt 42
          ]
        ])

    it "aborts on cyclic includes" $ do
      (decodeFile "test/resources/loop/foo.yaml" :: IO (Maybe Value)) `shouldThrow` anyException

    context "when file does not exist" $ do
      it "throws Left (InvalidYaml (Just (YamlException \"Yaml file not found: ...\")))" $ do
        (decodeFile "./does_not_exist.yaml" :: IO (Maybe Value)) `shouldThrow` isYamlFileNotFoundException

    context "with a 1K stack size limit" $ around_ inTempDirectory $ do
      context "with a large list" $ do
        it "succeeds" $ do
          let
            xs :: [Value]
            xs = replicate 5000 (Number 23)
          LB.writeFile "foo.yaml" (encode xs)
          decodeFile "foo.yaml" `shouldReturn` Just xs

  describe "decodeFileEither" $ do
    context "when file does not exist" $ do
      it "returns Left (InvalidYaml (Just (YamlException \"Yaml file not found: ...\")))" $ do
        (decodeFileEither "./does_not_exist.yaml" :: IO (Either ParseException Value)) >>=
          (`shouldSatisfy` either isYamlFileNotFoundException (const False))

  describe "decodeFileWithWarnings" $ around_ inTempDirectory $ do
    it "warns on duplicate keys" $ do
      writeFile "foo.yaml" [r|
        foo: 23
        foo: bar
        |]
      Right result <- decodeFileWithWarnings "foo.yaml"
      result `shouldBe` ([DuplicateKey [Key "foo"]], [yamlQQ|
        foo: bar
        |])

    it "warns on nested duplicate keys" $ do
      writeFile "foo.yaml" [r|
        foo:
          - 42
          - bar: 23
            bar: baz
        |]
      Right result <- decodeFileWithWarnings "foo.yaml"
      result `shouldBe` ([DuplicateKey [Key "foo", Index 1, Key "bar"]], [yamlQQ|
        foo:
          - 42
          - bar: baz
        |])

    context "when overriding a merged key" $ do
      it "does not warn" $ do
        writeFile "foo.yaml" [r|
          foo-1: &my-ref
            bar: 23
          foo-2:
            <<: *my-ref
            bar: 42
          |]
        Right result <- decodeFileWithWarnings "foo.yaml"
        result `shouldBe` ([], [yamlQQ|
          foo-1:
            bar: 23
          foo-2:
            bar: 42
          |])

      context "when overriding twice" $ do
        it "warns" $ do
          writeFile "foo.yaml" [r|
            foo-1: &my-ref
              bar: 23
            foo-2:
              <<: *my-ref
              bar: 42
              bar: 65
            |]
          Right result <- decodeFileWithWarnings "foo.yaml"
          result `shouldBe` ([DuplicateKey [Key "foo-2", Key "bar"]], [yamlQQ|
            foo-1:
              bar: 23
            foo-2:
              bar: 65
            |])

isYamlFileNotFoundException :: ParseException -> Bool
isYamlFileNotFoundException (InvalidYaml (Just (YamlException msg)))
  | "Yaml file not found: " `isPrefixOf` msg = True
isYamlFileNotFoundException _ = False
