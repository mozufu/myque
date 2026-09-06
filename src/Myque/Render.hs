{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Presentation.

Every function here is a pure projection of canonical state onto text.
Rendered output is never read back, so views cannot become authoritative:
deleting a generated document loses nothing.

Relationships are stored as canonical IDs but displayed as keys where an item
has one, which is what makes @key@ useful without making it identity. A
keyless item is displayed by an 'Abbrev' of its ID, which is computed against
the store so that it always resolves back to exactly one item. That
abbreviation is still for humans, so 'idLines' and 'jsonLines' exist alongside
it: a script enumerating the store gets full canonical identities in every
field.
-}
module Myque.Render
  ( Abbrev
  , abbrevOf
  , abbreviate
  , abbreviateWith
  , label
  , table
  , itemRows
  , idLines
  , jsonLines
  , itemDetail
  , mermaidGraph
  , markdownSummary
  ) where

import Data.Char (ord)
import Data.List (intercalate, sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Myque.Graph
  ( Edges
  , blockedBy
  , childrenOf
  , dependenciesOf
  , edgesOf
  , isReady
  )
import Myque.Item
  ( State (..)
  , WorkItem (..)
  , allStates
  , itemTitle
  , keyText
  , kindText
  , stateText
  )
import Myque.Store (Store (..), invalidFiles, storeItems)
import Myque.Timestamp (timestampText)
import Myque.Uuid (Uuid, uuidText)
import Numeric (showHex)

{- | The shortest unique abbreviation of every canonical ID in a store.

A UUIDv7's leading hex digits are its millisecond timestamp, so items created
in the same minute — or backdated to the same historical day by an import —
share a long common prefix. A fixed-width abbreviation therefore degenerates
to one repeated token exactly when a store holds many items, which is when a
listing has to distinguish them.
-}
newtype Abbrev = Abbrev (Map Uuid Text)

{- | Compute abbreviations for a store: for each ID, the shortest prefix no
other ID shares, never below 'abbrevFloor' characters and never past the
full 36. Group separators are included in the count, so an abbreviation is
always a valid selector prefix (see 'Myque.Store.isIdPrefix').
-}
abbreviate :: Store -> Abbrev
abbreviate store = abbreviateWith store []

{- | 'abbreviate', additionally disambiguating IDs the store does not hold
yet. @new@ names the item it just created, whose file the loaded store
predates, and that name has to be unique against everything already there.
-}
abbreviateWith :: Store -> [Uuid] -> Abbrev
abbreviateWith store extra = Abbrev (Map.fromList [(uuid, shortest uuid) | uuid <- uuids])
 where
  -- Undecodable files too: their names are identities a user will address,
  -- and an abbreviation that ignored them could resolve ambiguously.
  uuids = Map.keys (storeById store) <> map fst (invalidFiles store) <> extra
  texts = sort (map uuidText uuids)

  shortest uuid = go (candidates (uuidText uuid))
   where
    go [] = uuidText uuid
    go (c : cs)
      | unique c = c
      | otherwise = go cs

  -- Only lengths that leave a well-formed prefix: a UUID's separators sit at
  -- 8, 13, 18 and 23, and a prefix must not end on one.
  candidates full =
    [T.take n full | n <- [abbrevFloor .. 35], n `notElem` [9, 14, 19, 24]]

  unique prefix = length (filter (T.isPrefixOf prefix) texts) == 1

{- | The minimum abbreviation width. Eight digits is one UUID group, which is
what a reader recognises, and matches the width Git uses for the same job.
-}
abbrevFloor :: Int
abbrevFloor = 8

-- | An ID's abbreviation, or its full text when the store does not hold it.
abbrevOf :: Abbrev -> Uuid -> Text
abbrevOf (Abbrev m) uuid = Map.findWithDefault (uuidText uuid) uuid m

{- | How an item is named in output: its key when it has one, otherwise the
shortest abbreviation of its canonical ID that the store makes unambiguous.
-}
label :: Abbrev -> WorkItem -> Text
label abbrev item = maybe (abbrevOf abbrev (itemId item)) keyText (itemKey item)

-- | Name an ID by resolving it in the store, falling back to its abbreviation.
labelOf :: Store -> Abbrev -> Uuid -> Text
labelOf store abbrev uuid =
  maybe (abbrevOf abbrev uuid) (label abbrev) (Map.lookup uuid (storeById store))

{- | Render a header and rows as a left-aligned, space-padded table. The last
column is not padded, so output stays easy to pipe into other tools.
-}
table :: [Text] -> [[Text]] -> Text
table header rows
  | null rows = ""
  | otherwise = T.unlines (map renderRow (header : rows))
 where
  widths = map (maximum . map T.length) (columns (header : rows))
  renderRow cells = T.stripEnd (T.concat (zipWith pad widths cells))
  pad width cell = cell <> T.replicate (width + 2 - T.length cell) " "

-- | Transpose ragged rows into columns, padding short rows with @""@.
columns :: [[Text]] -> [[Text]]
columns rows = [map (cell i) rows | i <- [0 .. width - 1]]
 where
  width = maximum (0 : map length rows)
  cell i row = case drop i row of
    c : _ -> c
    [] -> ""

-- | The @KEY KIND STATE TITLE@ rows used by @list@ and @next@.
itemRows :: Store -> [WorkItem] -> Text
itemRows store items = table ["KEY", "KIND", "STATE", "TITLE"] (map row items)
 where
  edges = edgesOf store
  abbrev = abbreviate store
  row item =
    [ label abbrev item
    , kindText (itemKind item)
    , stateText (itemState item) <> readyMark item
    , itemTitle item
    ]
  readyMark item = if isReady store edges item then " (ready)" else ""

{- | One canonical 36-character ID per line, in the order the items were
given. An empty selection renders as the empty text, so a caller reading
lines sees no items rather than one blank one.
-}
idLines :: [WorkItem] -> Text
idLines = foldMap (\item -> uuidText (itemId item) <> "\n")

{- | NDJSON: one object per item, one line each. Every relationship field
carries canonical IDs, and @ready@ is included because it is derived rather
than stored, so a consumer never has to reimplement readiness or reparse
frontmatter.
-}
jsonLines :: Store -> [WorkItem] -> Text
jsonLines store = foldMap (\item -> object (itemFields store edges item) <> "\n")
 where
  edges = edgesOf store

-- | The JSON fields of one item, in a stable order.
itemFields :: Store -> Edges -> WorkItem -> [(Text, Json)]
itemFields store edges item =
  [ ("id", JString (uuidText (itemId item)))
  , ("key", maybe JNull (JString . keyText) (itemKey item))
  , ("kind", JString (kindText (itemKind item)))
  , ("state", JString (stateText (itemState item)))
  , ("title", JString (itemTitle item))
  , ("ready", JBool (isReady store edges item))
  , ("created", JString (timestampText (itemCreated item)))
  , ("updated", maybe JNull (JString . timestampText) (itemUpdated item))
  , ("closed", maybe JNull (JString . timestampText) (itemClosed item))
  , ("tags", JArray (map JString (itemTags item)))
  , ("parent", maybe JNull (JString . uuidText) (itemParent item))
  , ("children", ids (childrenOf edges item))
  , ("depends", ids (dependenciesOf edges item))
  , ("blocks", ids (blockedBy edges item))
  , ("related", ids (itemRelated item))
  , ("duplicate_of", maybe JNull (JString . uuidText) (itemDuplicateOf item))
  , ("supersedes", ids (itemSupersedes item))
  ]
 where
  ids = JArray . map (JString . uuidText)

-- | The JSON values the item projection needs. Numbers are not among them.
data Json
  = JString Text
  | JBool Bool
  | JNull
  | JArray [Json]

-- | Render an object on one line.
object :: [(Text, Json)] -> Text
object pairs = "{" <> T.intercalate "," [string k <> ":" <> value v | (k, v) <- pairs] <> "}"

-- | Render a JSON value.
value :: Json -> Text
value = \case
  JString t -> string t
  JBool True -> "true"
  JBool False -> "false"
  JNull -> "null"
  JArray vs -> "[" <> T.intercalate "," (map value vs) <> "]"

{- | A JSON string literal. Item titles come from arbitrary Markdown, so
quotes, backslashes and control characters all have to be escaped.
-}
string :: Text -> Text
string t = "\"" <> T.concatMap escape t <> "\""
 where
  escape c = case c of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _
      | c < ' ' || c == '\DEL' -> "\\u" <> T.justifyRight 4 '0' (T.pack (showHex (ord c) ""))
      | otherwise -> T.singleton c

-- | The full detail view of a single item, including derived relationships.
itemDetail :: Store -> Edges -> WorkItem -> Text
itemDetail store edges item =
  T.unlines $
    [ "id:       " <> uuidText (itemId item)
    ]
      <> field "key" (keyText <$> itemKey item)
      <> [ "kind:     " <> kindText (itemKind item)
         , "state:    " <> stateText (itemState item) <> readiness
         , "title:    " <> itemTitle item
         , "created:  " <> timestampText (itemCreated item)
         ]
      <> field "updated" (timestampText <$> itemUpdated item)
      <> field "closed" (timestampText <$> itemClosed item)
      <> list "tags" (itemTags item)
      <> field "parent" (labelOf store abbrev <$> itemParent item)
      <> list "children" (map (labelOf store abbrev) (childrenOf edges item))
      <> list "depends" (map (dependencyLabel store abbrev) (dependenciesOf edges item))
      <> list "blocks" (map (labelOf store abbrev) (blockedBy edges item))
      <> list "related" (map (labelOf store abbrev) (itemRelated item))
      <> field "duplicate_of" (labelOf store abbrev <$> itemDuplicateOf item)
      <> list "supersedes" (map (labelOf store abbrev) (itemSupersedes item))
      <> ["", T.stripEnd (T.stripStart (itemBody item))]
 where
  abbrev = abbreviate store
  readiness = if isReady store edges item then " (ready)" else ""
  field name = maybe [] (\v -> [pad name <> v])
  list _ [] = []
  list name vs = [pad name <> T.intercalate ", " vs]
  pad name = T.justifyLeft 10 ' ' (name <> ":")

{- | A dependency label annotated with whether it is satisfied, so an unready
item shows why.
-}
dependencyLabel :: Store -> Abbrev -> Uuid -> Text
dependencyLabel store abbrev uuid = case Map.lookup uuid (storeById store) of
  Nothing -> abbrevOf abbrev uuid <> " (missing)"
  Just dep
    | itemState dep == Done -> label abbrev dep
    | otherwise -> label abbrev dep <> " (" <> stateText (itemState dep) <> ")"

{- | A Mermaid flowchart of the parent and dependency relations. Parent edges
are dotted containment arrows; dependency edges point from dependency to
dependent, i.e. in execution order. Each node carries its state as a class,
and every state class is defined so the diagram renders standalone.
-}
mermaidGraph :: Store -> Text
mermaidGraph store =
  T.unlines $
    ["```mermaid", "flowchart TD"]
      <> map node items
      <> concatMap parentEdge items
      <> concatMap dependsEdge items
      <> map classDef allStates
      <> ["```"]
 where
  items = storeItems store
  edges = edgesOf store
  abbrev = abbreviate store
  node item =
    "  "
      <> nodeId (itemId item)
      <> "[\""
      <> escape (label abbrev item <> ": " <> itemTitle item)
      <> "\"]:::"
      <> stateText (itemState item)
  parentEdge item = ["  " <> nodeId p <> " -.-> " <> nodeId (itemId item) | Just p <- [itemParent item], known p]
  dependsEdge item =
    ["  " <> nodeId d <> " --> " <> nodeId (itemId item) | d <- dependenciesOf edges item, known d]
  classDef state = "  classDef " <> stateText state <> " " <> stateStyle state
  known uuid = Map.member uuid (storeById store)
  nodeId uuid = "n" <> T.filter (/= '-') (uuidText uuid)
  escape = T.replace "\"" "'"

-- | The Mermaid style of each state class.
stateStyle :: State -> Text
stateStyle = \case
  Open -> "fill:#f4f4f5,stroke:#71717a"
  Active -> "fill:#dbeafe,stroke:#2563eb"
  Blocked -> "fill:#fee2e2,stroke:#dc2626"
  Deferred -> "fill:#fef3c7,stroke:#d97706"
  Done -> "fill:#dcfce7,stroke:#16a34a"
  Cancelled -> "fill:#e4e4e7,stroke:#a1a1aa,stroke-dasharray:3 3"

{- | A Markdown summary grouped by state, most actionable first, for use as a
generated roadmap or backlog document. Empty states are omitted.
-}
markdownSummary :: Store -> Text
markdownSummary store =
  T.unlines $
    ["# Work items", ""]
      <> intercalate [""] (mapMaybe section [Active, Open, Blocked, Deferred, Done, Cancelled])
 where
  edges = edgesOf store
  abbrev = abbreviate store
  section state = case sortOn (label abbrev) (filter ((== state) . itemState) (storeItems store)) of
    [] -> Nothing
    items -> Just (("## " <> stateText state) : "" : map bullet items)
  bullet item =
    "- **"
      <> label abbrev item
      <> "** ("
      <> kindText (itemKind item)
      <> ") "
      <> itemTitle item
      <> tagSuffix item
      <> readySuffix item
  tagSuffix item
    | null (itemTags item) = ""
    | otherwise = " — " <> T.intercalate ", " (map (\t -> "`" <> t <> "`") (itemTags item))
  readySuffix item = if isReady store edges item then " _(ready)_" else ""
