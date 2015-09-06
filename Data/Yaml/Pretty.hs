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

import Control.Applicative ((<$>))
import Data.Yaml.Builder
import Data.Aeson.Types
import Data.Text (Text)
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Function (on)
import Data.List (sortBy)
import Data.ByteString (ByteString)
import Prelude hiding (null)

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
