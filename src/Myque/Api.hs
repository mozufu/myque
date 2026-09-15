{-# LANGUAGE OverloadedStrings #-}

module Myque.Api (itemApiValue, itemApiValueWith, apiGet, apiCreate, apiPut, migrateItem, retireItem, reopenItem, transitionItem) where

import Control.Monad (unless, when)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Myque.Graph (Edges, dependenciesOf, edgesOf, isReady)
import Myque.Item
import Myque.Storage
import Myque.Store
import Myque.Terminal (encodeTerminal)
import Myque.Timestamp (currentTimestamp)
import Myque.Uuid
import System.Directory (doesFileExist)
import System.FilePath (makeRelative, (</>))

-- | The supported machine representation of one item.
itemApiValue :: Store -> WorkItem -> Value
itemApiValue store = itemApiValueWith (edgesOf store) store

{- | As 'itemApiValue', with an edge index the caller already built. A consumer
rendering many items must not pay a full store re-index per item.
-}
itemApiValueWith :: Edges -> Store -> WorkItem -> Value
itemApiValueWith edges store item =
  object
    [ "api" .= ("myque/v2" :: Text)
    , "id" .= uuidText (itemId item)
    , "schema" .= itemSchema item
    , "state" .= stateText (itemState item)
    , "title" .= itemTitle item
    , "body" .= (if retired then Nothing else Just (bodyAfterTitle item))
    , "bodyAvailable" .= not retired
    , "consumers" .= itemConsumers item
    , "revision" .= digestText (fromMaybe (encodeItem item) (itemOriginal item))
    , "retired" .= retired
    , "ready" .= isReady store edges item
    , "history" .= (terminalHistory <$> Map.lookup (itemId item) (storeTerminals store))
    , "dependenciesDone" .= (null (storeLoadErrors store) && null (storeDuplicateIds store) && all (\uuid -> fmap itemState (Map.lookup uuid (storeById store)) == Just Done) (dependenciesOf edges item))
    ]
 where
  retired = Map.member (itemId item) (storeTerminals store)

require :: Either String a -> IO a
require = either (ioError . userError) pure

getItem :: Store -> Uuid -> IO WorkItem
getItem store uuid = do
  unless (null (storeLoadErrors store) && null (storeDuplicateIds store)) (ioError (userError "store contains corrupt or competing authorities; run myque check"))
  require (resolveSelector store (ById uuid))

apiGet :: Layout -> Uuid -> IO Value
apiGet layout uuid = withStorageLock (layoutRoot layout) $ do
  store <- loadStoreUnlocked layout
  itemApiValue store <$> getItem store uuid

-- Admission records hold request digest and identity only. Intent and item are
-- published in one recoverable transaction; retry never allocates a second ID.
apiCreate :: Layout -> Text -> Value -> IO Value
apiCreate layout token value = withStorageLock (layoutRoot layout) $ do
  when (T.null token) (ioError (userError "admission token must not be empty"))
  (title, kind, body, consumers) <- require $ parseEither (withObject "create" $ \o -> (,,,) <$> o .: "title" <*> o .: "kind" <*> o .: "body" <*> o .: "consumers") value
  parsedKind <- require (parseKind kind)
  require (validateConsumers consumers)
  when (T.null (T.strip title) || T.any (`elem` ['\r', '\n']) title) (ioError (userError "title must be a nonempty single line"))
  let path = layoutRoot layout </> ".tasks/admissions" </> T.unpack (digestText token) <> ".json"
      fingerprint = digestText (TE.decodeUtf8 (BL.toStrict (encode value)))
  present <- doesFileExist path
  uuid <-
    if present
      then do
        saved <- BS.readFile path >>= require . eitherDecodeStrict
        (old, identity) <- require (parseEither (withObject "admission" $ \o -> (,) <$> o .: "request" <*> o .: "id") saved)
        unless (old == fingerprint) (ioError (userError "admission token already used with different payload"))
        require (parseUuid identity)
      else do
        identity <- newUuidV7
        now <- currentTimestamp
        let fresh = setBodyAfterTitle body (newWorkItem identity parsedKind now title) {itemConsumers = consumers}
        _ <- require (decodeItem (encodeItem fresh))
        commitFiles
          (layoutRoot layout)
          [(itemPath layout identity, Just (encodeItem fresh)), (path, Just (TE.decodeUtf8 . BL.toStrict . encode $ object ["request" .= fingerprint, "id" .= uuidText identity]))]
        pure identity
  store <- loadStoreUnlocked layout
  itemApiValue store <$> getItem store uuid

apiPut :: Layout -> Uuid -> Text -> Value -> IO Value
apiPut layout uuid expected value = withStorageLock (layoutRoot layout) $ do
  store <- loadStoreUnlocked layout
  item <- getItem store uuid
  when (Map.member uuid (storeTerminals store)) (ioError (userError "retired item is immutable; reopen first"))
  unless (digestText (fromMaybe (encodeItem item) (itemOriginal item)) == expected) (ioError (userError "revision conflict; reload and retry"))
  (body, consumers) <- require $ parseEither (withObject "put" $ \o -> (,) <$> o .:? "body" <*> o .:? "consumers") value
  require (validateConsumers (fromMaybe Map.empty consumers))
  let patched = maybe item (`setBodyAfterTitle` item) body
      result = patched {itemConsumers = Map.union (fromMaybe Map.empty consumers) (itemConsumers item)}
  _ <- require (decodeItem (encodeItem result))
  commitFiles (layoutRoot layout) [(itemPath layout uuid, Just (encodeItem result))]
  updated <- loadStoreUnlocked layout
  itemApiValue updated <$> getItem updated uuid

migrateItem :: Layout -> Uuid -> IO ()
migrateItem layout uuid = withStorageLock (layoutRoot layout) $ do
  store <- loadStoreUnlocked layout
  item <- getItem store uuid
  when (Map.member uuid (storeTerminals store)) (ioError (userError "reopen before migration"))
  when (itemSchema item /= "work-item/v1") (ioError (userError "migration requires work-item/v1"))
  let raw = fromMaybe (encodeItem item) (itemOriginal item)
      -- Only the schema scalar changes. No frontmatter/body normalization.
      migrated = replaceVersion raw
  _ <- require (decodeItem migrated)
  history <- retainSnapshot (layoutRoot layout) (itemPath layout uuid) raw
  commitFiles
    (layoutRoot layout)
    [(itemPath layout uuid, Just migrated), (layoutRoot layout </> ".tasks/migrations" </> T.unpack (uuidText uuid) <> ".json", Just (TE.decodeUtf8 (BL.toStrict (encode history))))]
 where
  replaceVersion text = case T.breakOn "\n" text of
    (line, rest)
      | T.isPrefixOf "schema:" line -> T.replace "work-item/v1" "work-item/v2" line <> rest
      | T.null rest -> text
      | otherwise -> line <> "\n" <> replaceVersion (T.drop 1 rest)

retireItem :: Layout -> Uuid -> Text -> Map.Map Text Text -> IO ()
retireItem layout uuid evidence retained = withStorageLock (layoutRoot layout) $ do
  store <- loadStoreUnlocked layout
  item <- getItem store uuid
  unless (isTerminal (itemState item)) (ioError (userError "only done or cancelled items may retire"))
  when (Map.member uuid (storeTerminals store)) (ioError (userError "item already retired"))
  when (T.null (T.strip evidence)) (ioError (userError "closure evidence or cancellation reason is required"))
  unless (all (`Map.member` itemConsumers item) (Map.keys retained)) (ioError (userError "retention cannot introduce a consumer namespace"))
  require (validateConsumers retained)
  let minimal = (setBodyAfterTitle "" item) {itemConsumers = retained, itemOriginal = Nothing}
  _ <- require (decodeItem (encodeItem minimal))
  let path = itemPath layout uuid
  history <- retainSnapshot (layoutRoot layout) path (fromMaybe (encodeItem item) (itemOriginal item))
  let record = TerminalRecord minimal history evidence (T.pack (makeRelative (layoutRoot layout) path))
  commitFiles (layoutRoot layout) [(terminalPath layout uuid, Just (encodeTerminal record)), (path, Nothing)]

reopenItem :: Layout -> Uuid -> IO ()
reopenItem layout uuid = withStorageLock (layoutRoot layout) $ do
  store <- loadStoreUnlocked layout
  item <- getItem store uuid
  restored <- case Map.lookup uuid (storeTerminals store) of
    Nothing -> pure item
    Just terminal -> retrieveSnapshot (layoutRoot layout) (terminalHistory terminal) >>= require . decodeItem
  unless (itemId restored == uuid) (ioError (userError "historical identity mismatch"))
  now <- currentTimestamp
  let opened = restored {itemState = Open, itemClosed = Nothing, itemUpdated = Just now}
  commitFiles (layoutRoot layout) [(itemPath layout uuid, Just (encodeItem opened)), (terminalPath layout uuid, Nothing)]

transitionItem :: Layout -> Uuid -> Text -> State -> IO ()
transitionItem layout uuid expected state = withStorageLock (layoutRoot layout) $ do
  store <- loadStoreUnlocked layout
  item <- getItem store uuid
  when (Map.member uuid (storeTerminals store)) (ioError (userError "retired item requires verified reopen"))
  unless (digestText (fromMaybe (encodeItem item) (itemOriginal item)) == expected) (ioError (userError "revision conflict; recheck consumer eligibility"))
  now <- currentTimestamp
  let updated = item {itemState = state, itemClosed = if isTerminal state then Just now else Nothing, itemUpdated = Just now}
  commitFiles (layoutRoot layout) [(itemPath layout uuid, Just (encodeItem updated))]
