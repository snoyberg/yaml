{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Data.Yaml.Internal
    (
      ParseException(..)
    , prettyPrintParseException
    , Warning(..)
    , parse
    , decodeHelper
    , decodeHelper_
    , specialStrings
    , isNumeric
    , textToScientific
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), Applicative(..))
#endif
import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad (when, unless)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Internal (JSONPath, JSONPathElement(..))
import Data.Aeson.Types hiding (parse)
import qualified Data.Attoparsec.Text as Atto
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.Char (toUpper, ord)
import Data.List
import Data.Conduit ((.|), ConduitM, runConduit)
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HashSet
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Scientific (Scientific)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Typeable
import qualified Data.Vector as V

import qualified Text.Libyaml as Y
import Text.Libyaml hiding (encode, decode, encodeFile, decodeFile)

data ParseException = NonScalarKey
                    | UnknownAlias { _anchorName :: Y.AnchorName }
                    | UnexpectedEvent { _received :: Maybe Event
                                      , _expected :: Maybe Event
                                      }
                    | InvalidYaml (Maybe YamlException)
                    | AesonException String
                    | OtherParseException SomeException
                    | NonStringKeyAlias Y.AnchorName Value
                    | CyclicIncludes
    deriving (Show, Typeable)

instance Exception ParseException where
#if MIN_VERSION_base(4, 8, 0)
  displayException = prettyPrintParseException
#endif

-- | Alternative to 'show' to display a 'ParseException' on the screen.
--   Instead of displaying the data constructors applied to their arguments,
--   a more textual output is returned. For example, instead of printing:
--
-- > InvalidYaml (Just (YamlParseException {yamlProblem = "did not find expected ',' or '}'", yamlContext = "while parsing a flow mapping", yamlProblemMark = YamlMark {yamlIndex = 42, yamlLine = 2, yamlColumn = 12}})))
--
--   It looks more pleasant to print:
--
-- > YAML parse exception at line 2, column 12,
-- > while parsing a flow mapping:
-- > did not find expected ',' or '}'
--
-- Since 0.8.11
prettyPrintParseException :: ParseException -> String
prettyPrintParseException pe = case pe of
  NonScalarKey -> "Non scalar key"
  UnknownAlias anchor -> "Unknown alias `" ++ anchor ++ "`"
  UnexpectedEvent { _expected = mbExpected, _received = mbUnexpected } -> unlines
    [ "Unexpected event: expected"
    , "  " ++ show mbExpected
    , "but received"
    , "  " ++ show mbUnexpected
    ]
  InvalidYaml mbYamlError -> case mbYamlError of
    Nothing -> "Unspecified YAML error"
    Just yamlError -> case yamlError of
      YamlException s -> "YAML exception:\n" ++ s
      YamlParseException problem context mark -> concat
        [ "YAML parse exception at line " ++ show (yamlLine mark) ++
          ", column " ++ show (yamlColumn mark)
        , case context of
            "" -> ":\n"
            -- The context seems to include a leading "while" or similar.
            _  -> ",\n" ++ context ++ ":\n"
        , problem
        ]
  AesonException s -> "Aeson exception:\n" ++ s
  OtherParseException exc -> "Generic parse exception:\n" ++ show exc
  NonStringKeyAlias anchor value -> unlines
    [ "Non-string key alias:"
    , "  Anchor name: " ++ anchor
    , "  Value: " ++ show value
    ]
  CyclicIncludes -> "Cyclic includes"

defineAnchor :: Value -> String -> ConduitM e o Parse ()
defineAnchor value name = modify (modifyAnchors $ Map.insert name value)
  where
    modifyAnchors :: (Map String Value -> Map String Value) -> ParseState -> ParseState
    modifyAnchors f st =  st {parseStateAnchors = f (parseStateAnchors st)}

lookupAnchor :: String -> ConduitM e o Parse (Maybe Value)
lookupAnchor name = gets (Map.lookup name . parseStateAnchors)

data Warning = DuplicateKey JSONPath
    deriving (Eq, Show)

addWarning :: Warning -> ConduitM e o Parse ()
addWarning w = modify (modifyWarnings (w :))
  where
    modifyWarnings :: ([Warning] -> [Warning]) -> ParseState -> ParseState
    modifyWarnings f st =  st {parseStateWarnings = f (parseStateWarnings st)}

data ParseState = ParseState {
  parseStateAnchors :: Map String Value
, parseStateWarnings :: [Warning]
}

type Parse = ReaderT JSONPath (StateT ParseState (ResourceT IO))

requireEvent :: Event -> ConduitM Event o Parse ()
requireEvent e = do
    f <- CL.head
    unless (f == Just e) $ liftIO $ throwIO $ UnexpectedEvent f $ Just e

parse :: ConduitM Event o Parse Value
parse = do
    streamStart <- CL.head
    case streamStart of
        Nothing ->
            -- empty string input
            return Null
        Just EventStreamStart -> do
            documentStart <- CL.head
            case documentStart of
                Just EventStreamEnd ->
                    -- empty file input, comment only string/file input
                    return Null
                Just EventDocumentStart -> do
                    res <- parseO
                    requireEvent EventDocumentEnd
                    requireEvent EventStreamEnd
                    return res
                _ -> liftIO $ throwIO $ UnexpectedEvent documentStart Nothing
        _ -> liftIO $ throwIO $ UnexpectedEvent streamStart Nothing

parseScalar :: ByteString -> Anchor -> Style -> Tag
            -> ConduitM Event o Parse Text
parseScalar v a style tag = do
    let res = decodeUtf8With lenientDecode v
    mapM_ (defineAnchor (textToValue style tag res)) a
    return res

textToValue :: Style -> Tag -> Text -> Value
textToValue SingleQuoted _ t = String t
textToValue DoubleQuoted _ t = String t
textToValue _ StrTag t = String t
textToValue Folded _ t = String t
textToValue _ _ t
    | t `elem` ["null", "Null", "NULL", "~", ""] = Null
    | any (t `isLike`) ["y", "yes", "on", "true"] = Bool True
    | any (t `isLike`) ["n", "no", "off", "false"] = Bool False
    | Right x <- textToScientific t = Number x
    | otherwise = String t
  where x `isLike` ref = x `elem` [ref, T.toUpper ref, titleCased]
          where titleCased = toUpper (T.head ref) `T.cons` T.tail ref

textToScientific :: Text -> Either String Scientific
textToScientific = Atto.parseOnly (num <* Atto.endOfInput)
  where
    num = (fromInteger <$> ("0x" *> Atto.hexadecimal))
      <|> (fromInteger <$> ("0o" *> octal))
      <|> Atto.scientific

    octal = T.foldl' step 0 <$> Atto.takeWhile1 isOctalDigit
      where
        isOctalDigit c = (c >= '0' && c <= '7')
        step a c = (a `shiftL` 3) .|. fromIntegral (ord c - 48)

parseO :: ConduitM Event o Parse Value
parseO = do
    me <- CL.head
    case me of
        Just (EventScalar v tag style a) -> textToValue style tag <$> parseScalar v a style tag
        Just (EventSequenceStart _ _ a) -> parseS 0 a id
        Just (EventMappingStart _ _ a) -> parseM mempty a M.empty
        Just (EventAlias an) -> do
            m <- lookupAnchor an
            case m of
                Nothing -> liftIO $ throwIO $ UnknownAlias an
                Just v -> return v
        _ -> liftIO $ throwIO $ UnexpectedEvent me Nothing

parseS :: Int
       -> Y.Anchor
       -> ([Value] -> [Value])
       -> ConduitM Event o Parse Value
parseS !n a front = do
    me <- CL.peek
    case me of
        Just EventSequenceEnd -> do
            CL.drop 1
            let res = Array $ V.fromList $ front []
            mapM_ (defineAnchor res) a
            return res
        _ -> do
            o <- local (Index n :) parseO
            parseS (succ n) a $ front . (:) o

parseM :: Set Text
       -> Y.Anchor
       -> M.HashMap Text Value
       -> ConduitM Event o Parse Value
parseM mergedKeys a front = do
    me <- CL.head
    case me of
        Just EventMappingEnd -> do
            let res = Object front
            mapM_ (defineAnchor res) a
            return res
        _ -> do
            s <- case me of
                    Just (EventScalar v tag style a') -> parseScalar v a' style tag
                    Just (EventAlias an) -> do
                        m <- lookupAnchor an
                        case m of
                            Nothing -> liftIO $ throwIO $ UnknownAlias an
                            Just (String t) -> return t
                            Just v -> liftIO $ throwIO $ NonStringKeyAlias an v
                    _ -> liftIO $ throwIO $ UnexpectedEvent me Nothing

            (mergedKeys', al') <- local (Key s :) $ do
              o <- parseO
              let al = do
                      when (M.member s front && Set.notMember s mergedKeys) $ do
                          path <- reverse <$> ask
                          addWarning (DuplicateKey path)
                      return (Set.delete s mergedKeys, M.insert s o front)
              if s == pack "<<"
                         then case o of
                                  Object l  -> return (merge l)
                                  Array l -> return $ merge $ foldl' mergeObjects M.empty $ V.toList l
                                  _          -> al
                         else al
            parseM mergedKeys' a al'
    where mergeObjects al (Object om) = M.union al om
          mergeObjects al _           = al

          merge xs = (Set.fromList (M.keys xs \\ M.keys front), M.union front xs)

decodeHelper :: FromJSON a
             => ConduitM () Y.Event Parse ()
             -> IO (Either ParseException ([Warning], Either String a))
decodeHelper src = do
    -- This used to be tryAny, but the fact is that catching async
    -- exceptions is fine here. We'll rethrow them immediately in the
    -- otherwise clause.
    x <- try $ runResourceT $ runStateT (runReaderT (runConduit $ src .| parse) []) (ParseState Map.empty [])
    case x of
        Left e
            | Just pe <- fromException e -> return $ Left pe
            | Just ye <- fromException e -> return $ Left $ InvalidYaml $ Just (ye :: YamlException)
            | otherwise -> throwIO e
        Right (y, st) -> return $ Right (parseStateWarnings st, parseEither parseJSON y)

decodeHelper_ :: FromJSON a
              => ConduitM () Event Parse ()
              -> IO (Either ParseException ([Warning], a))
decodeHelper_ src = do
    x <- try $ runResourceT $ runStateT (runReaderT (runConduit $ src .| parse) []) (ParseState Map.empty [])
    return $ case x of
        Left e
            | Just pe <- fromException e -> Left pe
            | Just ye <- fromException e -> Left $ InvalidYaml $ Just (ye :: YamlException)
            | otherwise -> Left $ OtherParseException e
        Right (y, st) -> either
            (Left . AesonException)
            Right
            ((,) (parseStateWarnings st) <$> parseEither parseJSON y)

-- | Strings which must be escaped so as not to be treated as non-string scalars.
specialStrings :: HashSet.HashSet Text
specialStrings = HashSet.fromList $ T.words
    "y Y yes Yes YES n N no No NO true True TRUE false False FALSE on On ON off Off OFF null Null NULL ~ *"

isNumeric :: Text -> Bool
isNumeric = either (const False) (const True) . textToScientific
