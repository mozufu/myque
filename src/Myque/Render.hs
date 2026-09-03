{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Presentation.

Every function here is a pure projection of canonical state onto text.
Rendered output is never read back, so views cannot become authoritative:
deleting a generated document loses nothing.

Relationships are stored as canonical IDs but displayed as keys where an item
has one, which is what makes @key@ useful without making it identity.
-}
module Myque.Render
  ( label
  , shortId
  , table
  , itemRows
  , itemDetail
  , mermaidGraph
  , markdownSummary
  ) where

import Data.List (intercalate, sortOn)
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
import Myque.Store (Store (..), storeItems)
import Myque.Timestamp (timestampText)
import Myque.Uuid (Uuid, uuidText)

{- | How an item is named in output: its key when it has one, otherwise a
short prefix of its canonical ID.
-}
label :: WorkItem -> Text
label item = maybe (shortId (itemId item)) keyText (itemKey item)

-- | The first group of a UUID, enough to recognise an item by eye.
shortId :: Uuid -> Text
shortId = T.takeWhile (/= '-') . uuidText

-- | Name an ID by resolving it in the store, falling back to a short ID.
labelOf :: Store -> Uuid -> Text
labelOf store uuid = maybe (shortId uuid) label (Map.lookup uuid (storeById store))

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
  row item =
    [ label item
    , kindText (itemKind item)
    , stateText (itemState item) <> readyMark item
    , itemTitle item
    ]
  readyMark item = if isReady store edges item then " (ready)" else ""

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
      <> field "parent" (labelOf store <$> itemParent item)
      <> list "children" (map (labelOf store) (childrenOf edges item))
      <> list "depends" (map (dependencyLabel store) (dependenciesOf edges item))
      <> list "blocks" (map (labelOf store) (blockedBy edges item))
      <> list "related" (map (labelOf store) (itemRelated item))
      <> field "duplicate_of" (labelOf store <$> itemDuplicateOf item)
      <> list "supersedes" (map (labelOf store) (itemSupersedes item))
      <> ["", T.stripEnd (T.stripStart (itemBody item))]
 where
  readiness = if isReady store edges item then " (ready)" else ""
  field name = maybe [] (\v -> [pad name <> v])
  list _ [] = []
  list name vs = [pad name <> T.intercalate ", " vs]
  pad name = T.justifyLeft 10 ' ' (name <> ":")

{- | A dependency label annotated with whether it is satisfied, so an unready
item shows why.
-}
dependencyLabel :: Store -> Uuid -> Text
dependencyLabel store uuid = case Map.lookup uuid (storeById store) of
  Nothing -> shortId uuid <> " (missing)"
  Just dep
    | itemState dep == Done -> label dep
    | otherwise -> label dep <> " (" <> stateText (itemState dep) <> ")"

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
  node item =
    "  "
      <> nodeId (itemId item)
      <> "[\""
      <> escape (label item <> ": " <> itemTitle item)
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
  section state = case sortOn label (filter ((== state) . itemState) (storeItems store)) of
    [] -> Nothing
    items -> Just (("## " <> stateText state) : "" : map bullet items)
  bullet item =
    "- **"
      <> label item
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
