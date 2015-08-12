-- | Prettier YAML encoding.
module Data.Yaml.Pretty
    ( encodePretty
    , Config
    , getConfCompare
    , setConfCompare
    , defConfig
    ) where

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

data Config = Config
  { confCompare :: Text -> Text -> Ordering -- ^ Function used to sort keys in objects
  }

-- | The default configuration: do not sort objects.
defConfig :: Config
defConfig = Config mempty

getConfCompare :: Config -> Text -> Text -> Ordering
getConfCompare = confCompare

setConfCompare :: (Text -> Text -> Ordering) -> Config -> Config
setConfCompare cmp c = c { confCompare = cmp }

pretty :: Config -> Value -> YamlBuilder
pretty cfg = go
  where go (Object o) = mapping (sortBy (confCompare cfg `on` fst) $ HM.toList $ HM.map go o)
        go (Array a)  = array (fmap go $ V.toList a)
        go Null       = null
        go (String s) = string s
        go (Number n) = number n
        go (Bool b)   = bool b

-- | Configurable 'encode'.
encodePretty :: ToJSON a => Config -> a -> ByteString
encodePretty cfg = toByteString . pretty cfg . toJSON
