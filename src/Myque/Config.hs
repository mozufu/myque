{-# LANGUAGE OverloadedStrings #-}

module Myque.Config (Config (..), defaultConfig, parseConfig, renderConfig, readConfig) where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import System.FilePath (isAbsolute, normalise, splitDirectories, (</>))

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
  go _ acc []
    | validItemsDir (configItemsDir acc) = Right acc
    | otherwise = Left "unsafe or reserved storage.items path"
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

validItemsDir :: FilePath -> Bool
validItemsDir p = not (null p) && not (isAbsolute p) && normalise p == p && all (`notElem` ["..", ".", ".git"]) (splitDirectories p) && p /= ".tasks" && not (any (\reserved -> p == reserved || take (length reserved + 1) p == reserved <> "/") [".tasks/terminal", ".tasks/admissions", ".tasks/migrations"])

readConfig :: FilePath -> IO Config
readConfig root = do
  let path = root </> ".tasks/config.toml"
  present <- doesFileExist path
  if present then TIO.readFile path >>= either (ioError . userError) pure . parseConfig else pure defaultConfig
