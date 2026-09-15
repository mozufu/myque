{-# LANGUAGE OverloadedStrings #-}

{- | The YAML subset used for work-item frontmatter.

The work-item schema is a closed, flat mapping whose values are either
scalars or sequences of scalars, so the tracker parses exactly that subset
rather than depending on a general YAML implementation. Anything outside it
(nested mappings, flow collections, anchors, multi-document streams, block
scalars) is a parse error instead of being silently reinterpreted.

'plainSafe' is the single rule shared by reading and writing: a scalar is
written plainly exactly when it would parse back unchanged, and quoted
otherwise. 'parseDocument' keeps the Markdown body verbatim, so prose the
tracker does not understand survives a rewrite.
-}
module Myque.Frontmatter
  ( Node (..)
  , Frontmatter
  , fromFields
  , fields
  , lookupNode
  , duplicateKeys
  , Document (..)
  , parseDocument
  , renderDocument
  , renderFrontmatter
  , plainSafe
  , ownedKeys
  , consumerEntries
  ) where

import Control.Monad (when)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isAsciiLower, isDigit, isSpace)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Yaml qualified as Yaml

-- | A frontmatter value.
data Node
  = -- | @key: value@
    Scalar Text
  | -- | @key:@ followed by @- item@ lines
    Sequence [Text]
  | -- | @key:@ with nothing following it
    Empty
  | Raw Text
  deriving (Eq, Show)

-- | A flat mapping, in document order.
newtype Frontmatter = Frontmatter [(Text, Node)]
  deriving (Eq, Show)

-- | Build frontmatter from fields in the order they should be written.
fromFields :: [(Text, Node)] -> Frontmatter
fromFields = Frontmatter

-- | The fields, in document order.
fields :: Frontmatter -> [(Text, Node)]
fields (Frontmatter fs) = fs

-- | The first binding for a key, if any.
lookupNode :: Text -> Frontmatter -> Maybe Node
lookupNode k = lookup k . fields

-- | Keys bound more than once, sorted and reported once each.
duplicateKeys :: Frontmatter -> [Text]
duplicateKeys fm = dupes (sort (map fst (fields fm)))
 where
  dupes (a : b : rest)
    | a == b = a : dupes (dropWhile (== a) rest)
    | otherwise = dupes (b : rest)
  dupes _ = []

-- | A frontmatter block plus the Markdown body that follows it.
data Document = Document
  { docFrontmatter :: Frontmatter
  , docBody :: Text
  }
  deriving (Eq, Show)

{- | Parse a @---@-delimited frontmatter block and the Markdown body that
follows it. The body is preserved verbatim, so content the tracker does not
understand round-trips byte for byte.
-}
parseDocument :: Text -> Either String Document
parseDocument raw = do
  (block, body) <- splitBlock (stripBom raw)
  let blockText = T.take (T.length (dropLines 1 (stripBom raw)) - T.length body) (dropLines 1 (stripBom raw))
  fm <- parseOpenFields (T.take (T.length blockText - fenceLength blockText) blockText) block
  pure (Document fm body)
 where
  fenceLength text = T.length (last (T.lines text)) + if T.isSuffixOf "\n" text then 1 else 0

-- | Drop a leading UTF-8 byte-order mark, which editors sometimes add.
stripBom :: Text -> Text
stripBom t = fromMaybe t (T.stripPrefix "\65279" t)

{- | Separate the frontmatter lines from the body. The body is taken from the
original text rather than rejoined from lines, so its exact bytes — including
a trailing newline or its absence — survive.
-}
splitBlock :: Text -> Either String ([Text], Text)
splitBlock raw = case T.lines raw of
  opening : rest
    | isFence opening -> case break isFence rest of
        (_, []) -> Left "unterminated frontmatter block: missing closing ---"
        (block, _) -> Right (block, dropLines (length block + 2) raw)
  _ -> Left "missing frontmatter block: file must start with ---"

-- | Drop @n@ newline-terminated lines, keeping the remainder verbatim.
dropLines :: Int -> Text -> Text
dropLines 0 t = t
dropLines n t = case T.breakOn "\n" t of
  (_, "") -> ""
  (_, rest) -> dropLines (n - 1) (T.drop 1 rest)

-- | Whether a line is a @---@ document fence.
isFence :: Text -> Bool
isFence line = T.strip line == "---"

{- | The versioned reserved namespace. Consumers use a distinct unquoted
lowercase ASCII name, optionally containing digits, hyphens or underscores.
-}
ownedKeys :: [Text]
ownedKeys = ["schema", "id", "key", "kind", "state", "created", "updated", "closed", "tags", "parent", "depends", "blocks", "related", "duplicate_of", "supersedes"]

consumerEntries :: Frontmatter -> [(Text, Text)]
consumerEntries fm = [(k, raw) | (k, Raw raw) <- fields fm]

parseOpenFields :: Text -> [Text] -> Either String Frontmatter
parseOpenFields raw _ = do
  value <- either (Left . Yaml.prettyPrintParseException) Right (Yaml.decodeEither' (TE.encodeUtf8 raw) :: Either Yaml.ParseException Value)
  object <- case value of
    Object obj -> Right obj
    _ -> Left "frontmatter must be a YAML mapping"
  entries <- chunks (T.splitOn "\n" raw)
  let names = map fst entries
  when (length names /= length (KM.keys object)) (Left "duplicate or complex frontmatter key")
  when (any (\k -> Key.toText k `notElem` names) (KM.keys object)) (Left "quoted or complex frontmatter keys are unsupported")
  decoded <- traverse decodeEntry entries
  pure (Frontmatter decoded)
 where
  chunks [] = Right []
  chunks [""] = Right []
  chunks (line : rest)
    | T.null (T.strip line) || T.isPrefixOf "#" (T.stripStart line) = chunks rest
    | otherwise = do
        (key, _) <- splitKey 0 line
        when (not (validName key) || T.isPrefixOf " " line || T.isPrefixOf "\t" line) (Left "top-level keys must be unquoted namespace names")
        let (nested, remaining) = span continuation rest
            allLines = line : nested
            text = T.intercalate "\n" allLines <> if null remaining then "" else "\n"
        ((key, text) :) <$> chunks remaining
  continuation line = T.null (T.strip line) || T.isPrefixOf "#" (T.stripStart line) || T.isPrefixOf " " line || T.isPrefixOf "\t" line || T.isPrefixOf "- " line
  validName key = not (T.null key) && T.all (\c -> isAsciiLower c || isDigit c || c == '_' || c == '-') key
  decodeEntry (key, text)
    | key `elem` ownedKeys = do
        fm <- parseFields (zip [2 ..] (T.lines text))
        case fields fm of
          [entry] -> Right entry
          _ -> Left "invalid reserved field"
    | otherwise = Right (key, Raw text)

-- | Parse numbered frontmatter lines into a flat mapping.
parseFields :: [(Int, Text)] -> Either String Frontmatter
parseFields = fmap Frontmatter . go
 where
  go [] = Right []
  go ((lineNo, line) : rest)
    | blankOrComment line = go rest
    | isItem line = Left (at lineNo "sequence item outside of any field")
    | isSpace (T.head line) = Left (at lineNo "unexpected indentation")
    | otherwise = do
        (key, inline) <- splitKey lineNo line
        let (itemLines, rest') = span (isSequenceLine . snd) rest
            itemsOnly = filter (isItem . snd) itemLines
        items <- traverse parseItem itemsOnly
        node <- case (T.null (T.strip inline), null items) of
          (True, True) -> Right Empty
          (True, False) -> Right (Sequence items)
          (False, True) -> Scalar <$> parseScalar lineNo inline
          (False, False) -> Left (at lineNo "field has both an inline value and sequence items")
        ((key, node) :) <$> go rest'

  parseItem (lineNo, line) = parseScalar lineNo (T.drop 1 (T.stripStart line))
  isSequenceLine line = blankOrComment line || isItem line
  isItem line = T.isPrefixOf "-" (T.stripStart line) && not (isFence line)
  blankOrComment line = T.null (T.strip line) || T.isPrefixOf "#" (T.stripStart line)

-- | Split @key: value@, rejecting keys that are absent, quoted or complex.
splitKey :: Int -> Text -> Either String (Text, Text)
splitKey lineNo line = case T.breakOn ":" line of
  (_, "") -> Left (at lineNo "expected 'key: value'")
  (key, rest)
    | T.null (T.strip key) -> Left (at lineNo "empty field name")
    | T.any isSpace (T.strip key) -> Left (at lineNo "field name contains whitespace")
    | otherwise -> Right (T.strip key, T.drop 1 rest)

-- | Parse a scalar: plain, single-quoted or double-quoted.
parseScalar :: Int -> Text -> Either String Text
parseScalar lineNo raw = case T.uncons trimmed of
  Nothing -> Right ""
  Just ('"', _) -> quoted '"' unescapeDouble
  Just ('\'', _) -> quoted '\'' (Right . T.replace "''" "'")
  Just ('#', _) -> Right ""
  _
    | not (plainSafe plain) -> Left (at lineNo "unsupported YAML syntax in value")
    | otherwise -> Right plain
 where
  trimmed = T.strip raw
  plain = T.strip (fst (T.breakOn " #" trimmed))
  quoted q unescape = case T.stripPrefix (T.singleton q) trimmed of
    Nothing -> Left (at lineNo "malformed quoted value")
    Just body -> case T.breakOnEnd (T.singleton q) (fst (T.breakOn (T.pack [' ', '#']) body)) of
      ("", _) -> Left (at lineNo "unterminated quoted value")
      (inner, after)
        | not (T.null (T.strip after)) -> Left (at lineNo "trailing content after quoted value")
        | otherwise -> unescape (T.dropEnd 1 inner)

-- | Undo the double-quoted escapes the renderer can emit.
unescapeDouble :: Text -> Either String Text
unescapeDouble = go
 where
  go t = case T.breakOn "\\" t of
    (before, "") -> Right before
    (before, rest) -> case T.unpack (T.take 2 rest) of
      ['\\', c]
        | Just r <- replacement c -> (\tl -> before <> r <> tl) <$> go (T.drop 2 rest)
      _ -> Left ("unsupported escape in quoted value: " <> T.unpack (T.take 2 rest))
  replacement '"' = Just "\""
  replacement '\\' = Just "\\"
  replacement 'n' = Just "\n"
  replacement 't' = Just "\t"
  replacement _ = Nothing

{- | Whether a scalar can be written as a YAML plain scalar and read back
unchanged: no leading indicator character, no @": "@, no @" #"@, no control
or quoting characters, and no surrounding whitespace.
-}
plainSafe :: Text -> Bool
plainSafe v = case T.uncons v of
  Nothing -> False
  Just (c, _) ->
    not (indicator c)
      && T.strip v == v
      && not (": " `T.isInfixOf` v)
      && not (T.isSuffixOf ":" v)
      && not (" #" `T.isInfixOf` v)
      && not (T.any (`elem` ("\"'\\\n\t\r" :: String)) v)
 where
  indicator c = c `elem` ("-?:,[]{}#&*!|>%@`" :: String)

-- | Render a document: fenced frontmatter followed by the body.
renderDocument :: Document -> Text
renderDocument (Document fm body) = renderFrontmatter fm <> body

-- | Render the fenced frontmatter block, including a trailing newline.
renderFrontmatter :: Frontmatter -> Text
renderFrontmatter fm = "---\n" <> foldMap field (fields fm) <> "---\n"
 where
  field (key, Scalar v) = key <> ": " <> renderScalar v <> "\n"
  field (key, Sequence vs) = T.unlines ((key <> ":") : map (\v -> "  - " <> renderScalar v) vs)
  field (key, Empty) = key <> ":\n"
  field (_, Raw raw) = raw

-- | Write a scalar plainly when that round-trips, and double-quoted otherwise.
renderScalar :: Text -> Text
renderScalar v
  | plainSafe v = v
  | otherwise = "\"" <> escape v <> "\""
 where
  escape =
    T.replace "\n" "\\n"
      . T.replace "\t" "\\t"
      . T.replace "\r" "\\n"
      . T.replace "\"" "\\\""
      . T.replace "\\" "\\\\"

-- | Prefix a parse error with its line number.
at :: Int -> String -> String
at lineNo msg = "line " <> show lineNo <> ": " <> msg
