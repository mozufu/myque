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
  ) where

import Data.Bifunctor (first)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Myque.Item
  ( Key
  , WorkItem (..)
  , decodeItem
  , encodeItem
  , keyText
  , parseKey
  )
import Myque.Timestamp (timestampUtc)
import Myque.Uuid (Uuid, parseUuid, uuidText)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , removeFile
  )
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))

-- | Repository configuration. Deliberately minimal.
newtype Config = Config
  { configItemsDir :: FilePath
  -- ^ Items directory, relative to the repository root.
  }
  deriving (Eq, Show)

-- | The configuration assumed when @.tasks\/config.toml@ is absent.
defaultConfig :: Config
defaultConfig = Config {configItemsDir = ".tasks/items"}

-- | The configuration schema identifier.
configSchema :: Text
configSchema = "tracker-config/v1"

{- | Parse @.tasks\/config.toml@. Only the keys the tracker defines are
recognised; the parser accepts the flat @[storage]@ table of the
specification and rejects anything it cannot interpret.
-}
parseConfig :: Text -> Either String Config
parseConfig raw = go Nothing defaultConfig (zip [1 :: Int ..] (T.lines raw))
 where
  go _ acc [] = Right acc
  go table acc ((lineNo, line) : rest)
    | T.null stripped || T.isPrefixOf "#" stripped = go table acc rest
    | Just name <- T.stripSuffix "]" =<< T.stripPrefix "[" stripped = go (Just (T.strip name)) acc rest
    | otherwise = case T.breakOn "=" stripped of
        (_, "") -> Left (at lineNo "expected 'key = value'")
        (rawKey, rawValue) -> do
          value <- first (const (at lineNo "expected a quoted string value")) (tomlString (T.drop 1 rawValue))
          case (table, T.strip rawKey) of
            (Nothing, "schema")
              | value == configSchema -> go table acc rest
              | otherwise -> Left ("unknown config schema: " <> T.unpack value)
            (Just "storage", "items") -> go table acc {configItemsDir = T.unpack value} rest
            (_, key) -> Left (at lineNo ("unknown configuration key: " <> T.unpack (qualify table key)))
   where
    stripped = T.strip (fst (T.breakOn " #" line))
  qualify table key = maybe key (\t -> t <> "." <> key) table
  at lineNo msg = "config line " <> show lineNo <> ": " <> msg

-- | Parse a TOML basic or literal string.
tomlString :: Text -> Either String Text
tomlString raw = case T.uncons trimmed of
  Just ('"', _) -> unquote '"'
  Just ('\'', _) -> unquote '\''
  _ -> Left "not a quoted string"
 where
  trimmed = T.strip raw
  unquote q = case T.stripPrefix (T.singleton q) trimmed >>= T.stripSuffix (T.singleton q) of
    Nothing -> Left "unterminated string"
    Just inner -> Right inner

-- | Render a configuration file.
renderConfig :: Config -> Text
renderConfig cfg =
  T.unlines
    [ "schema = \"" <> configSchema <> "\""
    , ""
    , "[storage]"
    , "items = \"" <> T.pack (configItemsDir cfg) <> "\""
    ]

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
  }

-- | Every loaded item, oldest first by @created@ then by ID.
storeItems :: Store -> [WorkItem]
storeItems = sortOn (\i -> (timestampUtc (itemCreated i), uuidText (itemId i))) . Map.elems . storeById

-- | Load every @*.md@ file in the items directory.
loadStore :: Layout -> IO Store
loadStore layout = do
  let dir = layoutItemsDir layout
  present <- doesDirectoryExist dir
  entries <- if present then listDirectory dir else pure []
  let paths = sortOn id [dir </> e | e <- entries, takeExtension e == ".md"]
  loaded <- traverse readItem paths
  pure (indexStore layout loaded)
 where
  readItem path = do
    contents <- TIO.readFile path
    pure (path, decodeItem contents)

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
  createDirectoryIfMissing True (layoutItemsDir layout)
  let path = itemPath layout (itemId item)
  TIO.writeFile path (encodeItem item)
  pure path

-- | Remove an item's canonical file.
deleteItem :: Store -> WorkItem -> IO FilePath
deleteItem store item = do
  let path = Map.findWithDefault (itemPath (storeLayout store) (itemId item)) (itemId item) (storeSources store)
  removeFile path
  pure path

-- | A CLI reference to an item: a canonical ID or a human key.
data Selector
  = ById Uuid
  | ByKey Key
  deriving (Eq, Show)

{- | Parse a selector. A well-formed UUID is always read as a canonical ID, so
UUID resolution takes precedence over key lookup.
-}
parseSelector :: Text -> Either String Selector
parseSelector raw = case parseUuid raw of
  Right uuid -> Right (ById uuid)
  Left _ -> ByKey <$> parseKey raw

-- | Resolve a selector against the loaded store.
resolveSelector :: Store -> Selector -> Either String WorkItem
resolveSelector store sel = case sel of
  ById uuid -> byId uuid
  ByKey key -> case Map.lookup (keyText key) (storeByKey store) of
    Nothing -> Left ("no work item with key " <> T.unpack (keyText key))
    Just uuid
      | ambiguous (keyText key) -> Left ("key " <> T.unpack (keyText key) <> " resolves to more than one work item")
      | otherwise -> byId uuid
 where
  byId uuid = case Map.lookup uuid (storeById store) of
    Nothing -> Left ("no work item with id " <> T.unpack (uuidText uuid))
    Just item -> Right item
  ambiguous k = k `elem` map fst (storeDuplicateKeys store)
