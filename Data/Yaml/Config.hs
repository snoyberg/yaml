{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Some next-gen helper functions for the scaffolding's configuration system.
module Data.Yaml.Config
    ( applyCurrentEnv
    , getCurrentEnv
    , applyEnvValue
    , loadYamlSettings
    , EnvUsage
    , ignoreEnv
    , useEnv
    , requireEnv
    , useCustomEnv
    , requireCustomEnv
    ) where


#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
import Data.Monoid
#endif
import Data.Semigroup
import Data.List.NonEmpty (nonEmpty)
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Text (Text, pack)
import System.Environment (getEnvironment)
import Control.Arrow ((***))
import Control.Monad (forM)
import Control.Exception (throwIO)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Yaml as Y
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

newtype MergedValue = MergedValue { getMergedValue :: Value }

instance Semigroup MergedValue where
    MergedValue x <> MergedValue y = MergedValue $ mergeValues x y

-- | Left biased
mergeValues :: Value -> Value -> Value
mergeValues (Object x) (Object y) = Object $ H.unionWith mergeValues x y
mergeValues x _ = x

applyEnvValue :: Bool -- ^ require an environment variable to be present?
              -> H.HashMap Text Text -> Value -> Value
applyEnvValue requireEnv' env =
    goV
  where
    goV (Object o) = Object $ goV <$> o
    goV (Array a) = Array (goV <$> a)
    goV (String t1) = fromMaybe (String t1) $ do
        t2 <- T.stripPrefix "_env:" t1
        let (name, t3) = T.break (== ':') t2
            mdef = fmap parseValue $ T.stripPrefix ":" t3
        Just $ case H.lookup name env of
            Just val ->
                -- If the default value parses as a String, we treat the
                -- environment variable as a raw value and do not parse it.
                -- This means that things like numeric passwords just work.
                -- However, for originally numerical or boolean values (e.g.,
                -- port numbers), we still perform a normal YAML parse.
                --
                -- For details, see:
                -- https://github.com/yesodweb/yesod/issues/1061
                case mdef of
                    Just (String _) -> String val
                    _ -> parseValue val
            Nothing ->
                case mdef of
                    Just val | not requireEnv' -> val
                    _ -> Null
    goV v = v

    parseValue val = fromMaybe (String val) $ Y.decode $ encodeUtf8 val

getCurrentEnv :: IO (H.HashMap Text Text)
getCurrentEnv = fmap (H.fromList . map (pack *** pack)) getEnvironment

applyCurrentEnv :: Bool -- ^ require an environment variable to be present?
                -> Value -> IO Value
applyCurrentEnv requireEnv' orig = flip (applyEnvValue requireEnv') orig <$> getCurrentEnv

data EnvUsage = IgnoreEnv
              | UseEnv
              | RequireEnv
              | UseCustomEnv (H.HashMap Text Text)
              | RequireCustomEnv (H.HashMap Text Text)

ignoreEnv, useEnv, requireEnv :: EnvUsage
ignoreEnv = IgnoreEnv
useEnv = UseEnv
requireEnv = RequireEnv

useCustomEnv, requireCustomEnv :: H.HashMap Text Text -> EnvUsage
useCustomEnv = UseCustomEnv
requireCustomEnv = RequireCustomEnv

-- | Load the settings from the following three sources:
--
-- * Run time config files
--
-- * Run time environment variables
--
-- * The default compile time config file
loadYamlSettings
    :: FromJSON settings
    => [FilePath] -- ^ run time config files to use, earlier files have precedence
    -> [Value] -- ^ any other values to use, usually from compile time config. overridden by files
    -> EnvUsage
    -> IO settings
loadYamlSettings runTimeFiles compileValues envUsage = do
    runValues <- forM runTimeFiles $ \fp -> do
        eres <- Y.decodeFileEither fp
        case eres of
            Left e -> do
                putStrLn $ "loadYamlSettings: Could not parse file as YAML: " ++ fp
                throwIO e
            Right value -> return value

    value' <-
        case nonEmpty $ map MergedValue $ runValues ++ compileValues of
            Nothing -> error "loadYamlSettings: No configuration provided"
            Just ne -> return $ getMergedValue $ sconcat ne
    value <-
        case envUsage of
            IgnoreEnv            -> return $ applyEnvValue   False mempty value'
            UseEnv               ->          applyCurrentEnv False        value'
            RequireEnv           ->          applyCurrentEnv True         value'
            UseCustomEnv env     -> return $ applyEnvValue   False env    value'
            RequireCustomEnv env -> return $ applyEnvValue   True  env    value'

    case fromJSON value of
        Error s -> error $ "Could not convert to AppSettings: " ++ s
        Success settings -> return settings
