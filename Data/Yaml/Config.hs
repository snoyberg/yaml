{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Functionality for using YAML as configuration files
--
-- In particular, merging environment variables with yaml values
--
-- 'loadYamlSettings' is a high-level API for loading YAML and merging environment variables.
-- A yaml value of @_env:ENV_VAR:default@ will lookup the environment variable @ENV_VAR@.
--
-- On a historical note, this code was taken directly from the yesod web framework's configuration module.
module Data.Yaml.Config
    ( -- * High-level
      loadYamlSettings
    , loadYamlSettingsArgs
      -- ** EnvUsage
    , EnvUsage
    , ignoreEnv
    , useEnv
    , requireEnv
    , useCustomEnv
    , requireCustomEnv
      -- * Lower level
    , applyCurrentEnv
    , getCurrentEnv
    , applyEnvValue
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
import System.Environment (getArgs, getEnvironment)
import Control.Arrow ((***))
import Control.Monad (forM)
import Control.Exception (throwIO)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Yaml as Y
import qualified Data.Yaml.Include as YI
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

newtype MergedValue = MergedValue { getMergedValue :: Value }

instance Semigroup MergedValue where
    MergedValue x <> MergedValue y = MergedValue $ mergeValues x y

-- | Left biased
mergeValues :: Value -> Value -> Value
mergeValues (Object x) (Object y) = Object $ H.unionWith mergeValues x y
mergeValues x _ = x

-- | Override environment variable placeholders in the given @Value@ with
-- values from the environment.
--
-- If the first argument is @True@, then all placeholders _must_ be provided by
-- the actual environment. Otherwise, default values from the @Value@ will be
-- used.
--
-- @since 0.8.16
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

-- | Get the actual environment as a @HashMap@ from @Text@ to @Text@.
--
-- @since 0.8.16
getCurrentEnv :: IO (H.HashMap Text Text)
getCurrentEnv = fmap (H.fromList . map (pack *** pack)) getEnvironment

-- | A convenience wrapper around 'applyEnvValue' and 'getCurrentEnv'
--
-- @since 0.8.16
applyCurrentEnv :: Bool -- ^ require an environment variable to be present?
                -> Value -> IO Value
applyCurrentEnv requireEnv' orig = flip (applyEnvValue requireEnv') orig <$> getCurrentEnv

-- | Defines how we want to use the environment variables when loading a config
-- file. Use the smart constructors provided by this module.
--
-- @since 0.8.16
data EnvUsage = IgnoreEnv
              | UseEnv
              | RequireEnv
              | UseCustomEnv (H.HashMap Text Text)
              | RequireCustomEnv (H.HashMap Text Text)

-- | Do not use any environment variables, instead relying on defaults values
-- in the config file.
--
-- @since 0.8.16
ignoreEnv :: EnvUsage
ignoreEnv = IgnoreEnv

-- | Use environment variables when available, otherwise use defaults.
--
-- @since 0.8.16
useEnv :: EnvUsage
useEnv = UseEnv

-- | Do not use default values from the config file, but instead take all
-- overrides from the environment. If a value is missing, loading the file will
-- throw an exception.
--
-- @since 0.8.16
requireEnv :: EnvUsage
requireEnv = RequireEnv

-- | Same as 'useEnv', but instead of the actual environment, use the provided
-- @HashMap@ as the environment.
--
-- @since 0.8.16
useCustomEnv :: H.HashMap Text Text -> EnvUsage
useCustomEnv = UseCustomEnv

-- | Same as 'requireEnv', but instead of the actual environment, use the
-- provided @HashMap@ as the environment.
--
-- @since 0.8.16
requireCustomEnv :: H.HashMap Text Text -> EnvUsage
requireCustomEnv = RequireCustomEnv

-- | Load the settings from the following three sources:
--
-- * Run time config files
--
-- * Run time environment variables
--
-- * The default compile time config file
--
-- For example, to load up settings from @config/foo.yaml@ and allow overriding
-- from the actual environment, you can use:
--
-- > loadYamlSettings ["config/foo.yaml"] [] useEnv
--
-- @since 0.8.16
loadYamlSettings
    :: FromJSON settings
    => [FilePath] -- ^ run time config files to use, earlier files have precedence
    -> [Value] -- ^ any other values to use, usually from compile time config. overridden by files
    -> EnvUsage
    -> IO settings
loadYamlSettings runTimeFiles compileValues envUsage = do
    runValues <- forM runTimeFiles $ \fp -> do
        eres <- YI.decodeFileEither fp
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

-- | Same as @loadYamlSettings@, but get the list of runtime config files from
-- the command line arguments.
--
-- @since 0.8.17
loadYamlSettingsArgs
    :: FromJSON settings
    => [Value] -- ^ any other values to use, usually from compile time config. overridden by files
    -> EnvUsage -- ^ use environment variables
    -> IO settings
loadYamlSettingsArgs values env = do
    args <- getArgs
    loadYamlSettings args values env
