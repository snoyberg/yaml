{-# LANGUAGE CPP #-}
-- | Prettier YAML encoding.
--
-- Since 0.8.13
module Data.Yaml.Pretty
    ( encodePretty
    , Config
    , getConfCompare
    , setConfCompare
    , defConfig
    ) where

import Prelude hiding (null)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif
import Data.Text (Text)
import qualified Data.Vector as V

import Data.Yaml.Builder

-- |
-- Since 0.8.13
data Config = Config
  { confCompare :: Text -> Text -> Ordering -- ^ Function used to sort keys in objects
  }

-- | The default configuration: do not sort objects.
--
-- Since 0.8.13
defConfig :: Config
defConfig = Config mempty

-- |
-- Since 0.8.13
getConfCompare :: Config -> Text -> Text -> Ordering
getConfCompare = confCompare

-- |
-- Since 0.8.13
setConfCompare :: (Text -> Text -> Ordering) -> Config -> Config
setConfCompare cmp c = c { confCompare = cmp }

pretty :: Config -> Value -> YamlBuilder
pretty cfg = go
  where go (Object o) = mapping (sortBy (confCompare cfg `on` fst) $ HM.toList $ HM.map go o)
        go (Array a)  = array (go <$> V.toList a)
        go Null       = null
        go (String s) = string s
        go (Number n) = scientific n
        go (Bool b)   = bool b

-- | Configurable 'encode'.
--
-- Since 0.8.13
encodePretty :: ToJSON a => Config -> a -> ByteString
encodePretty cfg = toByteString . pretty cfg . toJSON
