{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Yaml.TH
  ( -- * Decoding
    yamlQQ
#if MIN_VERSION_template_haskell(2,9,0)
  , decodeFile
#endif
    -- * Re-exports from "Data.Yaml"
  , Value (..)
  , Parser
  , Object
  , Array
  , object
  , array
  , (.=)
  , (.:)
  , (.:?)
  , (.!=)
  , FromJSON (..)
  ) where

import           Data.Text.Encoding
import qualified Data.Text as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote

import           Data.Yaml hiding (decodeFile)

#if MIN_VERSION_template_haskell(2,9,0)
-- | Decode a @yaml@ file at compile time. Only available on GHC version @7.8.1@
-- or higher.
--
-- @since 0.8.19.0
--
-- ==== __Examples__
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
--
-- config :: Config
-- config = $$('decodeFile' "config.yaml")
-- @
decodeFile :: forall a. (Lift a, FromJSON a) => FilePath -> Q (TExp a)
decodeFile path = do
  addDependentFile path
  runIO (decodeFileEither path) >>= \case
    Left err -> fail (prettyPrintParseException err)
    Right x -> fmap TExp (lift (x :: a))
#endif

decodeValue :: String -> Either String Value
decodeValue = decodeEither . encodeUtf8 . T.pack

yamlExp :: String -> Q Exp
yamlExp input = case decodeValue input of
  Left err -> fail err
  Right a -> lift a

yamlQQ :: QuasiQuoter
yamlQQ = QuasiQuoter {
  quoteExp  = yamlExp
, quotePat  = notDefined "quotePat"
, quoteType = notDefined "quoteType"
, quoteDec  = notDefined "quoteDec"
} where
    notDefined name _ = fail (name ++ " is not defined for yamlQQ")
