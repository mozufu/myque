{-# LANGUAGE OverloadedStrings #-}

module Myque.Storage (withStorageLock, commitFiles, recoverFiles, digestText, retainSnapshot, retrieveSnapshot, repositoryIdentity, History (..)) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, when)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Myque.Config (Config (..), readConfig)
import Myque.History (History (..), validRelativePath)
import Myque.Item (decodeItem)
import Myque.Terminal (decodeTerminal)
import System.Directory
import System.Exit (ExitCode (..))
import System.FileLock (SharedExclusive (Exclusive), withFileLock)
import System.FilePath
import System.IO (hClose, openBinaryTempFile)
import System.Process (readProcessWithExitCode)

-- Git repository identity is the sorted set of root commit IDs, stable across clones.
-- A single canonical History definition lives in Myque.History.

digestText :: Text -> Text
digestText = T.pack . show . (hash :: BS.ByteString -> Digest SHA256) . TE.encodeUtf8

withStorageLock :: FilePath -> IO a -> IO a
withStorageLock root action = do
  createDirectoryIfMissing True (root </> ".tasks")
  withFileLock (root </> ".tasks/write.lock") Exclusive $ \_ -> recoverFiles root >> action

atomicBytes :: FilePath -> BS.ByteString -> IO ()
atomicBytes path bytes = do
  createDirectoryIfMissing True (takeDirectory path)
  bracket (openBinaryTempFile (takeDirectory path) ".myque-write") (\(tmp, h) -> do hClose h; exists <- doesFileExist tmp; when exists (removeFile tmp)) $ \(tmp, h) -> do
    BS.hPut h bytes
    hClose h
    renameFile tmp path

-- A durable intent is the authority until replay completes. All cooperating
-- readers acquire the same OS lock and replay first. Process death releases it.
commitFiles :: FilePath -> [(FilePath, Maybe Text)] -> IO ()
commitFiles root changes = do
  let relative = [(makeRelative root p, body) | (p, body) <- changes]
  writable <- writablePath root
  unless (all (writable . fst) relative) (ioError (userError "transaction path is outside canonical storage"))
  intents <- mapM (\(path, body) -> do old <- currentText (root </> path); pure (path, old, body)) relative
  atomicBytes (root </> ".tasks/transaction.json") (BL.toStrict (encode intents))
  recoverFiles root

{- | The canonical files a transaction may write: one item file under the
configured items directory, or one record in a MyQue-owned @.tasks@ directory.
The items directory comes from configuration, so a store that relocates it is
still writable, while an arbitrary repository path never is.
-}
writablePath :: FilePath -> IO (FilePath -> Bool)
writablePath root = do
  config <- readConfig root
  let items = splitDirectories (normalise (configItemsDir config))
      owned = [[".tasks", directory] | directory <- ["terminal", "admissions", "migrations"]]
  pure $ \p ->
    validRelativePath p && case reverse (splitDirectories p) of
      file : rest -> takeExtension file `elem` [".md", ".json"] && (reverse rest `elem` (items : owned))
      [] -> False

currentText :: FilePath -> IO (Maybe Text)
currentText path = do
  present <- doesFileExist path
  if present then Just . TE.decodeUtf8 <$> BS.readFile path else pure Nothing

recoverFiles :: FilePath -> IO ()
recoverFiles root = do
  let journal = root </> ".tasks/transaction.json"
  exists <- doesFileExist journal
  when exists $ do
    bytes <- BS.readFile journal
    changes <- either (ioError . userError . ("corrupt transaction journal: " <>)) pure (eitherDecodeStrict bytes :: Either String [(FilePath, Maybe Text, Maybe Text)])
    writable <- writablePath root
    unless (all (\(p, _, _) -> writable p) changes) (ioError (userError "unsafe journal path"))
    unless (length (Map.keys (Map.fromList [(p, ()) | (p, _, _) <- changes])) == length changes) (ioError (userError "duplicate journal path"))
    -- Validate the complete intended post-state before mutating anything, so a
    -- corrupt or tampered journal cannot half-apply.
    forM_ changes $ \(relative, old, new) -> do
      canonicalRoot <- canonicalizePath root
      canonical <- canonicalizePath (root </> relative)
      unless (writable (makeRelative canonicalRoot canonical)) (ioError (userError "journal path follows a symlink outside storage"))
      actual <- currentText (root </> relative)
      unless (actual == old || actual == new) (ioError (userError "journal recovery conflict: file edited after interrupted transaction"))
      case (splitDirectories relative, takeExtension relative, new) of
        (_, ".md", Just text) -> either (ioError . userError) (const (pure ())) (decodeItem text)
        ([".tasks", "terminal", _], _, Just text) -> either (ioError . userError) (const (pure ())) (decodeTerminal text)
        (_, _, Just text) -> case (eitherDecodeStrict (TE.encodeUtf8 text) :: Either String Value) of
          Left err -> ioError (userError ("invalid journal JSON payload: " <> err))
          Right _ -> pure ()
        _ -> pure ()
    forM_ changes $ \(relative, _, body) -> case body of
      Just text -> atomicBytes (root </> relative) (TE.encodeUtf8 text)
      Nothing -> do
        present <- doesFileExist (root </> relative)
        when present (removeFile (root </> relative))
    removeFile journal

git :: FilePath -> [String] -> String -> IO Text
git root args input = do
  (status, out, err) <- readProcessWithExitCode "git" (["-C", root] <> args) input
  case status of
    ExitSuccess -> pure (T.pack out)
    _ -> ioError (userError ("Git history unavailable: " <> err))

repositoryIdentity :: FilePath -> IO Text
repositoryIdentity root = T.unwords . sort . T.words <$> git root ["rev-list", "--max-parents=0", "--all"] ""

retainSnapshot :: FilePath -> FilePath -> Text -> IO History
retainSnapshot root path bytes = do
  identity <- repositoryIdentity root
  when (T.null identity) (ioError (userError "history requires a Git repository with an existing commit"))
  -- A private index avoids changing the user's index or committing unrelated work.
  blob <- T.strip <$> git root ["hash-object", "-w", "--stdin"] (T.unpack bytes)
  let relative = makeRelative root path
  writable <- writablePath root
  unless (writable relative && not (any (any (`elem` ['\t', '\n'])) (splitDirectories relative))) (ioError (userError "unsafe snapshot path"))
  tree <- snapshotTree root (splitDirectories relative) blob
  parent <- T.strip <$> git root ["rev-parse", "HEAD"] ""
  commit <- T.strip <$> git root ["commit-tree", T.unpack tree, "-p", T.unpack parent] "MyQue retained exact item snapshot\n"
  _ <- git root ["update-ref", "refs/myque/retained/" <> T.unpack commit, T.unpack commit] ""
  let h = History identity commit (T.pack relative) (digestText bytes)
  restored <- retrieveSnapshot root h
  unless (restored == bytes) (ioError (userError ("snapshot verification failed for " <> path)))
  pure h

snapshotTree :: FilePath -> [FilePath] -> Text -> IO Text
snapshotTree root [name] blob = T.strip <$> git root ["mktree"] ("100644 blob " <> T.unpack blob <> "\t" <> name <> "\n")
snapshotTree root (name : rest) blob = do
  subtree <- snapshotTree root rest blob
  T.strip <$> git root ["mktree"] ("040000 tree " <> T.unpack subtree <> "\t" <> name <> "\n")
snapshotTree _ [] _ = ioError (userError "empty snapshot path")
retrieveSnapshot :: FilePath -> History -> IO Text
retrieveSnapshot root h = do
  identity <- repositoryIdentity root
  unless (identity == historyRepository h) (ioError (userError "history repository identity mismatch; fetch the original repository roots and refs/myque/retained/*"))
  unless (T.length (historyCommit h) `elem` [40, 64]) (ioError (userError "history commit must be a full object ID"))
  bytes <- git root ["show", T.unpack (historyCommit h <> ":" <> historyPath h)] ""
  unless (digestText bytes == historyDigest h) (ioError (userError "history SHA-256 mismatch; item remains retired"))
  pure bytes
