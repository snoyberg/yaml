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

-- | Decode a YAML file at compile time. Only available on GHC version @7.8.1@
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
  x <- runIO $ decodeFileThrow path
  fmap TExp (lift (x :: a))

yamlExp :: String -> Q Exp
yamlExp input = do
  val <- runIO $ decodeThrow $ encodeUtf8 $ T.pack input
  lift (val :: Value)

-- | A @QuasiQuoter@ for YAML.
--
-- @since 0.8.28.0
--
-- ==== __Examples__
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import Data.Yaml.TH
--
-- value :: Value
-- value = [yamlQQ|
-- name: John Doe
-- age: 23
-- |]
-- @
yamlQQ :: QuasiQuoter
yamlQQ = QuasiQuoter {
  quoteExp  = yamlExp
, quotePat  = notDefined "quotePat"
, quoteType = notDefined "quoteType"
, quoteDec  = notDefined "quoteDec"
} where
    notDefined name _ = fail (name ++ " is not defined for yamlQQ")
