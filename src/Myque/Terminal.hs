{-# LANGUAGE OverloadedStrings #-}

module Myque.Terminal (TerminalRecord (..), History (..), encodeTerminal, decodeTerminal) where

import Control.Monad (unless, when)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Myque.History
import Myque.Item
import Myque.TerminalFields (terminalFields)
import Myque.Uuid (uuidText)
import System.FilePath (takeFileName)

-- The retained envelope contains the title, never the original opaque body.
data TerminalRecord = TerminalRecord {terminalItem :: WorkItem, terminalHistory :: History, terminalEvidence :: Text, terminalOriginalPath :: Text} deriving (Eq, Show)

encodeTerminal :: TerminalRecord -> Text
encodeTerminal r =
  TE.decodeUtf8 . BL.toStrict . encode $
    object
      ["schema" .= ("terminal-item/v1" :: Text), "metadata" .= encodeItem (terminalItem r), "history" .= terminalHistory r, "evidence" .= terminalEvidence r, "originalPath" .= terminalOriginalPath r]

decodeTerminal :: Text -> Either String TerminalRecord
decodeTerminal text = do
  value <- eitherDecodeStrict (TE.encodeUtf8 text)
  parseEither
    ( withObject "terminal-item/v1" $ \o -> do
        schema <- o .: "schema"
        unless (all (`elem` terminalFields) (KM.keys o) && length (KM.keys o) == length terminalFields) (fail "terminal record fields do not match canonical schema")
        unless (schema == ("terminal-item/v1" :: Text)) (fail "unsupported terminal schema")
        raw <- o .: "metadata"
        item <- either fail pure (decodeItem raw)
        unless (isTerminal (itemState item)) (fail "terminal record has nonterminal state")
        when (T.null (T.strip (itemTitle item))) (fail "terminal title is required")
        unless (T.null (bodyAfterTitle item)) (fail "terminal record must not retain a full body")
        history <- o .: "history"
        evidence <- o .: "evidence"
        when (T.null (T.strip evidence)) (fail "terminal evidence/reason is required")
        original <- o .: "originalPath"
        unless (original == historyPath history && takeFileName (T.unpack original) == T.unpack (uuidText (itemId item)) <> ".md") (fail "terminal original path must match history and identity")
        pure (TerminalRecord item history evidence original)
    )
    value
