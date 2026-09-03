{-# LANGUAGE OverloadedStrings #-}

{- | The @work-item\/v1@ model and its canonical Markdown encoding.

A t'WorkItem' is exactly the frontmatter schema of the specification plus the
Markdown body. Decoding is strict: the schema is closed, so unknown
frontmatter fields, unknown schema versions, malformed timestamps and
inconsistent @closed@ metadata are decode errors rather than warnings.

The body is stored verbatim. 'itemTitle' reads a title out of it by
convention (the first ATX level-1 heading) without making the title part of
the schema.
-}
module Myque.Item
  ( Kind (..)
  , kindText
  , parseKind
  , allKinds
  , State (..)
  , stateText
  , parseState
  , allStates
  , isTerminal
  , Key
  , keyText
  , parseKey
  , WorkItem (..)
  , newWorkItem
  , itemTitle
  , setTitle
  , schemaVersion
  , decodeItem
  , encodeItem
  , outgoingIds
  ) where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Myque.Frontmatter
  ( Document (..)
  , Frontmatter
  , Node (..)
  , fields
  , fromFields
  , lookupNode
  , parseDocument
  , renderDocument
  )
import Myque.Frontmatter qualified as FM
import Myque.Timestamp (Timestamp, parseTimestamp, timestampText)
import Myque.Uuid (Uuid, isUuidV7, parseUuid, uuidText)

-- | The schema identifier every canonical file carries.
schemaVersion :: Text
schemaVersion = "work-item/v1"

-- | The semantic role of an item. Carries no storage or identity semantics.
data Kind
  = Task
  | Issue
  | Bug
  | Milestone
  | Epic
  | Followup
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Every kind, in schema order.
allKinds :: [Kind]
allKinds = [minBound .. maxBound]

-- | The wire form of a kind.
kindText :: Kind -> Text
kindText Task = "task"
kindText Issue = "issue"
kindText Bug = "bug"
kindText Milestone = "milestone"
kindText Epic = "epic"
kindText Followup = "followup"

-- | Parse a kind, rejecting anything outside the schema enumeration.
parseKind :: Text -> Either String Kind
parseKind t = case lookup t [(kindText k, k) | k <- allKinds] of
  Just k -> Right k
  Nothing -> Left ("invalid kind: " <> T.unpack t <> " (expected one of " <> enumeration kindText allKinds <> ")")

-- | The lifecycle state of an item.
data State
  = Open
  | Active
  | Blocked
  | Deferred
  | Done
  | Cancelled
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Every state, in schema order.
allStates :: [State]
allStates = [minBound .. maxBound]

-- | The wire form of a state.
stateText :: State -> Text
stateText Open = "open"
stateText Active = "active"
stateText Blocked = "blocked"
stateText Deferred = "deferred"
stateText Done = "done"
stateText Cancelled = "cancelled"

-- | Parse a state, rejecting anything outside the schema enumeration.
parseState :: Text -> Either String State
parseState t = case lookup t [(stateText s, s) | s <- allStates] of
  Just s -> Right s
  Nothing -> Left ("invalid state: " <> T.unpack t <> " (expected one of " <> enumeration stateText allStates <> ")")

{- | Whether a state is terminal. Terminal states require @closed@;
non-terminal states forbid it.
-}
isTerminal :: State -> Bool
isTerminal s = s == Done || s == Cancelled

-- | Render an enumeration for an error message.
enumeration :: (a -> Text) -> [a] -> String
enumeration render = T.unpack . T.intercalate ", " . map render

-- | A human-readable alias. Never canonical identity.
newtype Key = Key Text
  deriving (Eq, Ord)

instance Show Key where
  show (Key t) = T.unpack t

-- | The textual form of a key.
keyText :: Key -> Text
keyText (Key t) = t

{- | Parse a key. Keys are single-token aliases so that they can be used as
CLI selectors and rendered in tables; they must not look like UUIDs.
-}
parseKey :: Text -> Either String Key
parseKey raw
  | T.null trimmed = Left "key must not be empty"
  | T.any isSpace trimmed = Left ("key must not contain whitespace: " <> T.unpack trimmed)
  | Right _ <- parseUuid trimmed = Left ("key must not be a UUID: " <> T.unpack trimmed)
  | otherwise = Right (Key trimmed)
 where
  trimmed = T.strip raw

-- | A work item: the closed frontmatter schema plus a verbatim body.
data WorkItem = WorkItem
  { itemId :: Uuid
  , itemKey :: Maybe Key
  , itemKind :: Kind
  , itemState :: State
  , itemCreated :: Timestamp
  , itemUpdated :: Maybe Timestamp
  , itemClosed :: Maybe Timestamp
  , itemTags :: [Text]
  , itemParent :: Maybe Uuid
  , itemDepends :: [Uuid]
  , itemBlocks :: [Uuid]
  , itemRelated :: [Uuid]
  , itemDuplicateOf :: Maybe Uuid
  , itemSupersedes :: [Uuid]
  , itemBody :: Text
  }
  deriving (Eq, Show)

{- | A new @open@ item with the given identity, title and creation time, and
no relationships.
-}
newWorkItem :: Uuid -> Kind -> Timestamp -> Text -> WorkItem
newWorkItem uuid kind created title =
  WorkItem
    { itemId = uuid
    , itemKey = Nothing
    , itemKind = kind
    , itemState = Open
    , itemCreated = created
    , itemUpdated = Nothing
    , itemClosed = Nothing
    , itemTags = []
    , itemParent = Nothing
    , itemDepends = []
    , itemBlocks = []
    , itemRelated = []
    , itemDuplicateOf = Nothing
    , itemSupersedes = []
    , itemBody = "\n# " <> T.strip title <> "\n"
    }

{- | The title by convention: the first ATX level-1 heading in the body. Falls
back to the key, then to the canonical ID, so every item renders with
something.
-}
itemTitle :: WorkItem -> Text
itemTitle item = fromMaybe fallback (firstHeading (itemBody item))
 where
  fallback = maybe (uuidText (itemId item)) keyText (itemKey item)

-- | The first @# @ heading of a Markdown body, if there is one.
firstHeading :: Text -> Maybe Text
firstHeading body = case mapMaybe heading (T.lines body) of
  h : _ -> Just h
  [] -> Nothing
 where
  heading line = T.strip <$> T.stripPrefix "# " (T.stripStart line)

{- | Replace the first level-1 heading, or prepend one when the body has
none. Everything else in the body is untouched.
-}
setTitle :: Text -> WorkItem -> WorkItem
setTitle title item = item {itemBody = rewrite (itemBody item)}
 where
  replacement = "# " <> T.strip title
  rewrite body = case break isHeading (T.lines body) of
    (_, []) -> "\n" <> replacement <> "\n" <> body
    (before, _ : after) -> T.unlines (before <> [replacement] <> after)
  isHeading line = T.isPrefixOf "# " (T.stripStart line)

-- | Every ID referenced by an item, deduplicated. Used for dangling checks.
outgoingIds :: WorkItem -> [Uuid]
outgoingIds item =
  nub $
    catMaybes [itemParent item, itemDuplicateOf item]
      <> itemDepends item
      <> itemBlocks item
      <> itemRelated item
      <> itemSupersedes item

-- | The frontmatter fields the schema permits, in canonical write order.
knownFields :: [Text]
knownFields =
  [ "schema"
  , "id"
  , "key"
  , "kind"
  , "state"
  , "created"
  , "updated"
  , "closed"
  , "tags"
  , "parent"
  , "depends"
  , "blocks"
  , "related"
  , "duplicate_of"
  , "supersedes"
  ]

-- | Decode a canonical Markdown file.
decodeItem :: Text -> Either String WorkItem
decodeItem raw = do
  Document fm body <- parseDocument raw
  case FM.duplicateKeys fm of
    dup : _ -> Left ("duplicate frontmatter field: " <> T.unpack dup)
    [] -> pure ()
  case filter (`notElem` knownFields) (map fst (fields fm)) of
    unknown : _ -> Left ("unsupported frontmatter field: " <> T.unpack unknown)
    [] -> pure ()
  schema <- requiredScalar "schema" fm
  when (schema /= schemaVersion) (Left ("unknown schema version: " <> T.unpack schema))
  uuid <- requiredScalar "id" fm >>= field "id" . canonicalId
  key <- optional "key" fm parseKey
  kind <- requiredScalar "kind" fm >>= field "kind" . parseKind
  state <- requiredScalar "state" fm >>= field "state" . parseState
  created <- requiredScalar "created" fm >>= field "created" . parseTimestamp
  updated <- optional "updated" fm parseTimestamp
  closed <- optional "closed" fm parseTimestamp
  tags <- tagList fm
  parent <- optional "parent" fm parseUuid
  depends <- uuidList "depends" fm
  blocks <- uuidList "blocks" fm
  related <- uuidList "related" fm
  duplicateOf <- optional "duplicate_of" fm parseUuid
  supersedes <- uuidList "supersedes" fm
  case (isTerminal state, closed) of
    (True, Nothing) -> Left ("state " <> T.unpack (stateText state) <> " requires 'closed'")
    (False, Just _) -> Left ("non-terminal state " <> T.unpack (stateText state) <> " must not have 'closed'")
    _ -> pure ()
  pure
    WorkItem
      { itemId = uuid
      , itemKey = key
      , itemKind = kind
      , itemState = state
      , itemCreated = created
      , itemUpdated = updated
      , itemClosed = closed
      , itemTags = tags
      , itemParent = parent
      , itemDepends = depends
      , itemBlocks = blocks
      , itemRelated = related
      , itemDuplicateOf = duplicateOf
      , itemSupersedes = supersedes
      , itemBody = body
      }

-- | Parse a canonical identity: a UUID that is also a UUIDv7.
canonicalId :: Text -> Either String Uuid
canonicalId t = do
  uuid <- parseUuid t
  if isUuidV7 uuid
    then Right uuid
    else Left ("canonical id is not a UUIDv7: " <> T.unpack t)

-- | Prefix a field error with the field name.
field :: Text -> Either String a -> Either String a
field name = either (Left . (("field '" <> T.unpack name <> "': ") <>)) Right

-- | A mandatory scalar field.
requiredScalar :: Text -> Frontmatter -> Either String Text
requiredScalar name fm = case lookupNode name fm of
  Nothing -> Left ("missing required field: " <> T.unpack name)
  Just (Scalar v) | not (T.null v) -> Right v
  Just _ -> Left ("field '" <> T.unpack name <> "' must be a non-empty scalar")

-- | An optional scalar field, absent when unset or explicitly empty.
optional :: Text -> Frontmatter -> (Text -> Either String a) -> Either String (Maybe a)
optional name fm parse = case lookupNode name fm of
  Nothing -> Right Nothing
  Just Empty -> Right Nothing
  Just (Scalar v)
    | T.null (T.strip v) -> Right Nothing
    | otherwise -> Just <$> field name (parse v)
  Just (Sequence _) -> Left ("field '" <> T.unpack name <> "' must be a scalar, not a sequence")

-- | An optional sequence-of-UUID field.
uuidList :: Text -> Frontmatter -> Either String [Uuid]
uuidList name fm = fmap nub (sequenceField name fm >>= traverse (field name . parseUuid))

-- | The @tags@ field: a sequence of non-empty strings, deduplicated.
tagList :: Frontmatter -> Either String [Text]
tagList fm = do
  raw <- sequenceField "tags" fm
  traverse validate (nub raw)
 where
  validate t
    | T.null (T.strip t) = Left "field 'tags': tag must not be empty"
    | T.any isSpace t = Left ("field 'tags': tag must not contain whitespace: " <> T.unpack t)
    | otherwise = Right t

{- | An optional sequence field. A scalar is rejected rather than coerced, so
@depends: 019a...@ is an error instead of a silent single-element list.
-}
sequenceField :: Text -> Frontmatter -> Either String [Text]
sequenceField name fm = case lookupNode name fm of
  Nothing -> Right []
  Just Empty -> Right []
  Just (Sequence vs) -> Right vs
  Just (Scalar _) -> Left ("field '" <> T.unpack name <> "' must be a sequence, not a scalar")

-- | Encode a work item as its canonical Markdown file.
encodeItem :: WorkItem -> Text
encodeItem item = renderDocument (Document (frontmatterOf item) (itemBody item))

-- | Build the frontmatter block for an item, in canonical field order.
frontmatterOf :: WorkItem -> Frontmatter
frontmatterOf item =
  fromFields $
    [ ("schema", Scalar schemaVersion)
    , ("id", Scalar (uuidText (itemId item)))
    ]
      <> scalarField "key" (keyText <$> itemKey item)
      <> [ ("kind", Scalar (kindText (itemKind item)))
         , ("state", Scalar (stateText (itemState item)))
         , ("created", Scalar (timestampText (itemCreated item)))
         ]
      <> scalarField "updated" (timestampText <$> itemUpdated item)
      <> scalarField "closed" (timestampText <$> itemClosed item)
      <> listField "tags" (itemTags item)
      <> scalarField "parent" (uuidText <$> itemParent item)
      <> listField "depends" (map uuidText (itemDepends item))
      <> listField "blocks" (map uuidText (itemBlocks item))
      <> listField "related" (map uuidText (itemRelated item))
      <> scalarField "duplicate_of" (uuidText <$> itemDuplicateOf item)
      <> listField "supersedes" (map uuidText (itemSupersedes item))
 where
  scalarField name = maybe [] (\v -> [(name, Scalar v)])
  listField _ [] = []
  listField name vs = [(name, Sequence vs)]
