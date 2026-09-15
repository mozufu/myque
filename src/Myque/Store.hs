{-# LANGUAGE OverloadedStrings #-}

{- | Canonical on-disk storage.

State lives in one Markdown file per work item under the configured items
directory, named after the item's canonical ID. Nothing else is
authoritative: there is no registry file, no sequence counter and no index,
so two branches can each add a file without touching shared state.

'loadStore' reads every item into an in-memory t'Store' that is a derived,
disposable index — 'saveItem' writes canonical files, never the index.
-}
module Myque.Store
  ( Config (..)
  , defaultConfig
  , parseConfig
  , renderConfig
  , Layout (..)
  , discoverLayout
  , initLayout
  , itemPath
  , Store (..)
  , loadStore
  , storeItems
  , LoadError (..)
  , formatLoadError
  , saveItem
  , deleteItem
  , Selector (..)
  , parseSelector
  , resolveSelector
  , invalidFiles
  , TerminalRecord (..)
  , History (..)
  , terminalDirectory
  , terminalPath
  , loadStoreUnlocked
  , saveItems
  ) where

import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.Char (isHexDigit)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Myque.Config
import Myque.Item
  ( Key
  , WorkItem (..)
  , decodeItem
  , encodeItem
  , keyText
  , parseKey
  , validateConsumers
  )
import Myque.Storage (commitFiles, withStorageLock)
import Myque.Terminal
import Myque.Timestamp (timestampUtc)
import Myque.Uuid (Uuid, parseUuid, uuidText)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))

-- | A resolved tracker location.
data Layout = Layout
  { layoutRoot :: FilePath
  -- ^ Repository root: the directory containing @.tasks@.
  , layoutConfig :: Config
  }
  deriving (Eq, Show)

-- | The directory holding canonical item files.
layoutItemsDir :: Layout -> FilePath
layoutItemsDir l = layoutRoot l </> configItemsDir (layoutConfig l)

-- | The canonical path of an item: @\<items dir\>\/\<uuid\>.md@.
itemPath :: Layout -> Uuid -> FilePath
itemPath l uuid = layoutItemsDir l </> T.unpack (uuidText uuid) <> ".md"

terminalDirectory :: Layout -> FilePath
terminalDirectory layout = layoutRoot layout </> ".tasks/terminal"

terminalPath :: Layout -> Uuid -> FilePath
terminalPath layout uuid = terminalDirectory layout </> T.unpack (uuidText uuid) <> ".json"

{- | Find the tracker by walking up from a starting directory looking for
@.tasks@, then load its configuration.
-}
discoverLayout :: FilePath -> IO (Either String Layout)
discoverLayout start = do
  found <- ascend start
  case found of
    Nothing -> pure (Left "no tracker found: no .tasks directory in this directory or any parent (run 'myque init')")
    Just root -> do
      let configFile = root </> ".tasks" </> "config.toml"
      present <- doesFileExist configFile
      if present
        then fmap (fmap (Layout root)) (parseConfig <$> TIO.readFile configFile)
        else pure (Right (Layout root defaultConfig))
 where
  ascend dir = do
    present <- doesDirectoryExist (dir </> ".tasks")
    if present
      then pure (Just dir)
      else
        let parent = takeDirectory dir
         in if parent == dir then pure Nothing else ascend parent

-- | Create @.tasks\/config.toml@ and the items directory under a root.
initLayout :: FilePath -> IO Layout
initLayout root = do
  let layout = Layout root defaultConfig
      configFile = root </> ".tasks" </> "config.toml"
  createDirectoryIfMissing True (layoutItemsDir layout)
  present <- doesFileExist configFile
  if present then pure () else TIO.writeFile configFile (renderConfig defaultConfig)
  pure layout

-- | A file that could not be turned into a work item.
data LoadError = LoadError
  { loadErrorPath :: FilePath
  , loadErrorMessage :: String
  }
  deriving (Eq, Show)

-- | Render a load error as one diagnostic line.
formatLoadError :: LoadError -> Text
formatLoadError (LoadError path msg) = T.pack path <> ": " <> T.pack msg

{- | A derived, disposable index over the canonical files: items by ID, key
lookup, and the reverse dependency and child edges.

Duplicate IDs and keys are retained so that validation can report them; the
lookup maps keep the first file in path order and 'storeDuplicateIds' /
'storeDuplicateKeys' record the collisions.
-}
data Store = Store
  { storeLayout :: Layout
  , storeById :: Map Uuid WorkItem
  , storeByKey :: Map Text Uuid
  , storeSources :: Map Uuid FilePath
  -- ^ The file each loaded item came from.
  , storeDuplicateIds :: [(Uuid, [FilePath])]
  , storeDuplicateKeys :: [(Text, [Uuid])]
  , storeMismatchedFiles :: [(FilePath, Uuid)]
  -- ^ Files whose basename is not the item's canonical ID.
  , storeLoadErrors :: [LoadError]
  , storeTerminals :: Map Uuid TerminalRecord
  }

-- | Every loaded item, oldest first by @created@ then by ID.
storeItems :: Store -> [WorkItem]
storeItems = sortOn (\i -> (timestampUtc (itemCreated i), uuidText (itemId i))) . Map.elems . storeById

-- | Load every @*.md@ file in the items directory.
loadStore :: Layout -> IO Store
loadStore layout = withStorageLock (layoutRoot layout) (loadStoreUnlocked layout)

loadStoreUnlocked :: Layout -> IO Store
loadStoreUnlocked layout = do
  paths <- files (layoutItemsDir layout) ".md"
  terminals <- files (terminalDirectory layout) ".json"
  loaded <- traverse readItem paths
  retired <- traverse readTerminal terminals
  let store = indexStore layout (loaded <> [(p, terminalItem <$> result) | (p, result) <- retired])
  pure store {storeTerminals = Map.fromList [(itemId (terminalItem r), r) | (_, Right r) <- retired]}
 where
  files dir extension = do
    present <- doesDirectoryExist dir
    entries <- if present then listDirectory dir else pure []
    pure (sortOn id [dir </> e | e <- entries, takeExtension e == extension])
  readItem path = do
    contents <- TE.decodeUtf8' <$> BS.readFile path
    pure (path, either (Left . show) decodeItem contents)
  readTerminal path = do
    contents <- TE.decodeUtf8' <$> BS.readFile path
    pure (path, either (Left . show) decodeTerminal contents)

-- | Build the derived index from decode results in path order.
indexStore :: Layout -> [(FilePath, Either String WorkItem)] -> Store
indexStore layout loaded =
  Store
    { storeLayout = layout
    , storeById = Map.fromList [(itemId i, i) | (_, i) <- reverse oks]
    , storeByKey = Map.fromList [(keyText k, itemId i) | (_, i) <- reverse oks, Just k <- [itemKey i]]
    , storeSources = Map.fromList [(itemId i, p) | (p, i) <- reverse oks]
    , storeDuplicateIds = duplicatesOf [(itemId i, p) | (p, i) <- oks]
    , storeDuplicateKeys = duplicatesOf [(keyText k, itemId i) | (_, i) <- oks, Just k <- [itemKey i]]
    , storeMismatchedFiles = [(p, itemId i) | (p, i) <- oks, takeBaseName p /= T.unpack (uuidText (itemId i))]
    , storeLoadErrors = [LoadError p e | (p, Left e) <- loaded]
    , storeTerminals = Map.empty
    }
 where
  oks = [(p, i) | (p, Right i) <- loaded]

  -- Keys bound more than once, with their values in encounter order.
  duplicatesOf :: (Ord k) => [(k, v)] -> [(k, [v])]
  duplicatesOf pairs =
    [ (k, vs)
    | (k, vs) <- Map.toAscList (Map.fromListWith (flip (<>)) [(k, [v]) | (k, v) <- pairs])
    , length vs > 1
    ]

-- | Write an item to its canonical path, creating the items directory.
saveItem :: Layout -> WorkItem -> IO FilePath
saveItem layout item = do
  saveItems layout [item]
  pure (itemPath layout (itemId item))

{- | Compare the original exact bytes under the shared storage lock. New
identities require absence; terminal records can only be changed by reopen.
-}
saveItems :: Layout -> [WorkItem] -> IO ()
saveItems layout items = withStorageLock (layoutRoot layout) $ do
  mapM_ checkItem items
  commitFiles (layoutRoot layout) [(itemPath layout (itemId item), Just (encodeItem item)) | item <- items]
 where
  checkItem item = do
    retired <- doesFileExist (terminalPath layout (itemId item))
    when retired (ioError (userError "retired item is immutable; use myque reopen"))
    let path = itemPath layout (itemId item)
    exists <- doesFileExist path
    actual <- if exists then Just . TE.decodeUtf8 <$> BS.readFile path else pure Nothing
    unless (actual == itemOriginal item) (ioError (userError "write conflict: reload the item and retry"))
    either (ioError . userError) pure (validateConsumers (itemConsumers item))
    case decodeItem (encodeItem item) of
      Left err -> ioError (userError err)
      Right _ -> pure ()

-- | UUIDs cannot be deleted: incoming links and history must remain resolvable.
deleteItem :: Store -> WorkItem -> IO FilePath
deleteItem _ _ = ioError (userError "deletion is unsupported; close/cancel then retire with evidence")

-- | A CLI reference to an item: a canonical ID, an abbreviated ID, or a key.
data Selector
  = -- | A full canonical UUID.
    ById Uuid
  | {- | An abbreviation of a canonical ID, as written. The same text may
    also be a well-formed key, so resolution tries IDs first and keys
    second, and keeps the original case for the key lookup.
    -}
    ByPrefix Text
  | -- | A human key that cannot be read as an ID abbreviation.
    ByKey Key
  deriving (Eq, Show)

{- | Parse a selector. A well-formed UUID is always read as a canonical ID
and an abbreviation of one is read as a prefix, so ID resolution takes
precedence over key lookup (specification §17.2).
-}
parseSelector :: Text -> Either String Selector
parseSelector raw = case parseUuid raw of
  Right uuid -> Right (ById uuid)
  Left _
    | isIdPrefix trimmed -> Right (ByPrefix trimmed)
    | otherwise -> ByKey <$> parseKey raw
 where
  trimmed = T.strip raw

{- | Whether the text is a proper prefix of the canonical @8-4-4-4-12@ UUID
form: hex digits, with group separators exactly where a full UUID has them.
-}
isIdPrefix :: Text -> Bool
isIdPrefix t =
  not (T.null t)
    && T.length t < 36
    && and (zipWith positionOk [0 :: Int ..] (T.unpack (T.toLower t)))
 where
  positionOk i c
    | i `elem` [8, 13, 18, 23] = c == '-'
    | otherwise = isHexDigit c

{- | Resolve a selector against the loaded store. Ambiguity fails closed: an
abbreviation matching several items names every candidate rather than
picking one. An abbreviation of a file that failed to load resolves to that
file's error, so a malformed item is reported as broken, not as absent.
-}
resolveSelector :: Store -> Selector -> Either String WorkItem
resolveSelector store sel = case sel of
  ById uuid -> byId uuid
  ByKey key -> byKey (keyText key)
  ByPrefix prefix -> case matching prefix of
    [uuid] -> byId uuid
    [] -> case Map.lookup prefix (storeByKey store) of
      Just _ -> byKey prefix
      Nothing ->
        Left
          ( "no work item with an id starting "
              <> T.unpack prefix
              <> ", and no key "
              <> T.unpack prefix
          )
    candidates ->
      Left
        ( "ambiguous item abbreviation "
            <> T.unpack prefix
            <> ": "
            <> T.unpack (T.intercalate ", " (map uuidText candidates))
        )
 where
  -- Loaded items and undecodable files alike, so an abbreviation never
  -- silently resolves past a broken file that shares its prefix. IDs are
  -- stored lowercase, so the abbreviation is folded for the comparison only.
  matching prefix =
    filter
      (T.isPrefixOf (T.toLower prefix) . uuidText)
      (Map.keys (storeById store) <> map fst (invalidFiles store))

  byKey k = case Map.lookup k (storeByKey store) of
    Nothing -> Left ("no work item with key " <> T.unpack k)
    Just uuid
      | k `elem` map fst (storeDuplicateKeys store) ->
          Left ("key " <> T.unpack k <> " resolves to more than one work item")
      | otherwise -> byId uuid

  byId uuid = case Map.lookup uuid (storeById store) of
    Just item -> Right item
    Nothing -> case lookup uuid (invalidFiles store) of
      Just (LoadError path msg) ->
        Left
          ( "work item "
              <> T.unpack (uuidText uuid)
              <> " exists but is invalid, so it cannot be addressed: "
              <> path
              <> ": "
              <> msg
          )
      Nothing -> Left ("no work item with id " <> T.unpack (uuidText uuid))

{- | The load errors of files whose name is a canonical ID, keyed by that ID.
A file that failed to decode has no item to index, but its name still names
the identity the user will address it by.
-}
invalidFiles :: Store -> [(Uuid, LoadError)]
invalidFiles store =
  [ (uuid, err)
  | err@(LoadError path _) <- storeLoadErrors store
  , Right uuid <- [parseUuid (T.pack (takeBaseName path))]
  ]
