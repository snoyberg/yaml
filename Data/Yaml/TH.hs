{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.Yaml.TH
  ( -- * Decoding
    decodeFile
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

import Data.Yaml hiding (decodeFile)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

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
