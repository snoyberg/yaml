{-# LANGUAGE OverloadedStrings #-}
import Data.Yaml.Parser
import Data.Conduit
import Text.Libyaml
import Control.Applicative
import Data.Text (Text)

data Person = Person
    { name :: !Text
    , age :: !Int
    }
    deriving (Show)

instance FromYaml Person where
    fromYaml = withMapping "Person" $ \o -> Person
        <$> o .: "name"
        <*> o .: "age"

main :: IO ()
main = do
    persons <- readYamlFile "/tmp/foo.yaml"
    mapM_ print (persons :: [Person])
