{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module Data.Yaml.Include (
  decodeFile
, decodeFileEither
, decodeFileWithWarnings
) where

#if !MIN_VERSION_directory(1, 2, 3)
import Control.Exception (handleJust)
import Control.Monad (guard)
import System.IO.Error (ioeGetFileName, ioeGetLocation, isDoesNotExistError)
#endif

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Aeson (FromJSON)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import System.Directory
import System.FilePath

import Data.Yaml.Internal (ParseException(..), Warning(..), decodeHelper_, decodeHelper)
import Text.Libyaml hiding (decodeFile)
import qualified Text.Libyaml as Y

eventsFromFile
    :: MonadResource m
    => FilePath
    -> ConduitM i Event m ()
eventsFromFile = go []
  where
    go :: MonadResource m => [FilePath] -> FilePath -> ConduitM i Event m ()
    go seen fp = do
        cfp <- liftIO $ handleNotFound $ canonicalizePath fp
        when (cfp `elem` seen) $ do
            liftIO $ throwIO CyclicIncludes
        Y.decodeFile cfp .| do
            awaitForever $ \event -> case event of
                EventScalar f (UriTag "!include") _ _ -> do
                    let includeFile = takeDirectory cfp </> unpack (decodeUtf8 f)
                    go (cfp : seen) includeFile .| CL.filter (`notElem` irrelevantEvents)
                _ -> yield event

    irrelevantEvents = [EventStreamStart, EventDocumentStart, EventDocumentEnd, EventStreamEnd]

#if !MIN_VERSION_directory(1, 2, 3)
    handleNotFound = handleJust
        (\e -> do
            guard (isDoesNotExistError e)
            guard (ioeGetLocation e == "canonicalizePath")
            ioeGetFileName e)
        (throwIO . YamlException . ("Yaml file not found: " ++))
#else
    handleNotFound = id
#endif

-- | Like `Data.Yaml.decodeFile` but with support for relative and absolute
-- includes.
--
-- The syntax for includes follows the form:
--
-- > somekey: !include ./somefile.yaml
decodeFile
    :: FromJSON a
    => FilePath
    -> IO (Maybe a)
decodeFile fp = (fmap snd <$> decodeHelper (eventsFromFile fp)) >>= either throwIO (return . either (const Nothing) id)

-- | Like `Data.Yaml.decodeFileEither` but with support for relative and
-- absolute includes.
--
-- The syntax for includes follows the form:
--
-- > somekey: !include ./somefile.yaml
decodeFileEither
    :: FromJSON a
    => FilePath
    -> IO (Either ParseException a)
decodeFileEither = fmap (fmap snd) . decodeFileWithWarnings

-- | A version of `decodeFileEither` that returns warnings along with the parse
-- result.
--
-- @since 0.10.0
decodeFileWithWarnings
    :: FromJSON a
    => FilePath
    -> IO (Either ParseException ([Warning], a))
decodeFileWithWarnings = decodeHelper_ . eventsFromFile
