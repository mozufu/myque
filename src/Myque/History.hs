{-# LANGUAGE OverloadedStrings #-}

module Myque.History (History (..), validObjectId, validRelativePath) where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Myque.TerminalFields (historyFields)
import System.FilePath (isAbsolute, normalise, splitDirectories)

data History = History {historyRepository :: Text, historyCommit :: Text, historyPath :: Text, historyDigest :: Text} deriving (Eq, Show)
instance ToJSON History where
  toJSON h = object ["repository" .= historyRepository h, "commit" .= historyCommit h, "path" .= historyPath h, "digest" .= historyDigest h]
instance FromJSON History where
  parseJSON = withObject "history" $ \o -> do
    unless (length (KM.keys o) == length historyFields && all (`elem` historyFields) (KM.keys o)) (fail "history fields do not match canonical schema")
    h <- History <$> o .: "repository" <*> o .: "commit" <*> o .: "path" <*> o .: "digest"
    unless (not (null (T.words (historyRepository h))) && all validObjectId (T.words (historyRepository h))) (fail "history repository must contain full root object IDs")
    unless (validObjectId (historyCommit h)) (fail "history commit must be a full hexadecimal object ID")
    unless (T.length (historyDigest h) == 64 && hex (historyDigest h)) (fail "history digest must be SHA-256 hex")
    unless (validRelativePath (T.unpack (historyPath h))) (fail "history path must be repository relative")
    pure h
validObjectId :: Text -> Bool
validObjectId t = T.length t `elem` [40, 64] && hex t
hex :: Text -> Bool
hex = T.all (`elem` ("0123456789abcdef" :: String))
validRelativePath :: FilePath -> Bool
validRelativePath p = not (null p) && not (isAbsolute p) && normalise p == p && all (`notElem` ["..", ".", ".git"]) (splitDirectories p) && not (any (`elem` ['\t', '\n', '\r', '\0']) p)
