{-# LANGUAGE OverloadedStrings #-}

{- | Repository validation.

'validate' collects every finding the specification requires a checker to
detect: identity problems, duplicate aliases, dangling references, cycles,
inconsistent state metadata, and schema violations. Schema-level problems
(unknown fields, malformed timestamps, invalid enumerations) are already
rejected when a file is decoded, so those surface here as 'FileUnreadable'
findings carrying the decoder's message.

Findings are values, not printed text, so both the CLI and the tests inspect
the same result.
-}
module Myque.Validate
  ( Finding (..)
  , findingText
  , validate
  ) where

import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Myque.Graph (dependencyCycles, parentCycles)
import Myque.Item (State (..), WorkItem (..))
import Myque.Store (LoadError (..), Store (..), storeItems)
import Myque.Uuid (Uuid, uuidText)
import System.FilePath (takeFileName)

-- | A single validation problem.
data Finding
  = {- | A file that could not be decoded: malformed frontmatter, unknown
    schema version, unsupported field, invalid enumeration or timestamp,
    non-UUIDv7 identity, or inconsistent @closed@ metadata.
    -}
    FileUnreadable FilePath String
  | -- | The same canonical ID appears in more than one file.
    DuplicateId Uuid [FilePath]
  | -- | The same key is claimed by more than one item.
    DuplicateKey Text [Uuid]
  | -- | A file's name is not its item's canonical ID.
    FilenameMismatch FilePath Uuid
  | -- | A relationship field references an item that does not exist.
    DanglingReference Uuid Text Uuid
  | -- | An item names itself as its parent.
    SelfParent Uuid
  | -- | An item depends on itself.
    SelfDependency Uuid
  | -- | The parent relation contains a cycle.
    ParentCycle [Uuid]
  | -- | The dependency relation contains a cycle.
    DependencyCycle [Uuid]
  | {- | An item declares itself a duplicate of an active item while staying
    active itself.
    -}
    DuplicateStillActive Uuid
  | -- | @updated@ precedes @created@, or @closed@ precedes @created@.
    TimestampOutOfOrder Uuid Text
  deriving (Eq, Show)

-- | Render a finding as one diagnostic line.
findingText :: Finding -> Text
findingText f = case f of
  FileUnreadable path msg -> T.pack (takeFileName path) <> ": " <> T.pack msg
  DuplicateId uuid paths ->
    "duplicate canonical id: " <> uuidText uuid <> " in " <> T.intercalate ", " (map (T.pack . takeFileName) paths)
  DuplicateKey key ids -> "duplicate key: " <> key <> " claimed by " <> T.intercalate ", " (map uuidText ids)
  FilenameMismatch path uuid ->
    "filename/id mismatch: " <> T.pack (takeFileName path) <> " holds " <> uuidText uuid
  DanglingReference from fld to -> uuidText from <> ": dangling " <> fld <> " reference to " <> uuidText to
  SelfParent uuid -> uuidText uuid <> ": item is its own parent"
  SelfDependency uuid -> uuidText uuid <> ": item depends on itself"
  ParentCycle members -> "parent cycle: " <> T.intercalate " -> " (map uuidText members)
  DependencyCycle members -> "dependency cycle: " <> T.intercalate " -> " (map uuidText members)
  DuplicateStillActive uuid -> uuidText uuid <> ": declares duplicate_of but is not in a closed state"
  TimestampOutOfOrder uuid fld -> uuidText uuid <> ": " <> fld <> " precedes created"

-- | Every finding in a loaded store, in a stable order.
validate :: Store -> [Finding]
validate store =
  concat
    [ map (\(LoadError p m) -> FileUnreadable p m) (storeLoadErrors store)
    , [DuplicateId uuid (sort paths) | (uuid, paths) <- storeDuplicateIds store]
    , [DuplicateKey key (sort ids) | (key, ids) <- storeDuplicateKeys store]
    , [FilenameMismatch path uuid | (path, uuid) <- storeMismatchedFiles store]
    , concatMap itemFindings (storeItems store)
    , map ParentCycle (parentCycles store)
    , map DependencyCycle (dependencyCycles store)
    ]
 where
  known uuid = Map.member uuid (storeById store)

  itemFindings item =
    concat
      [ [DanglingReference self fld target | (fld, target) <- references item, not (known target)]
      , [SelfParent self | itemParent item == Just self]
      , [SelfDependency self | self `elem` itemDepends item || self `elem` itemBlocks item]
      , [ DuplicateStillActive self
        | Just canonical <- [itemDuplicateOf item]
        , canonical /= self
        , itemState item `notElem` [Done, Cancelled, Deferred]
        ]
      , [TimestampOutOfOrder self "updated" | Just u <- [itemUpdated item], u < itemCreated item]
      , [TimestampOutOfOrder self "closed" | Just c <- [itemClosed item], c < itemCreated item]
      ]
   where
    self = itemId item

  references item =
    concat
      [ [("parent", p) | p <- maybeToList (itemParent item)]
      , [("depends", d) | d <- itemDepends item]
      , [("blocks", b) | b <- itemBlocks item]
      , [("related", r) | r <- itemRelated item]
      , [("duplicate_of", d) | d <- maybeToList (itemDuplicateOf item)]
      , [("supersedes", s) | s <- itemSupersedes item]
      ]
