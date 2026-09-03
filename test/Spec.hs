{-# LANGUAGE OverloadedStrings #-}

{- | Behavioural tests for the work-item tracker.

The suite is organised around the invariants of @work-item\/v1@: identity is
an immutable UUIDv7, relationships reference IDs rather than keys, the
schema is closed, terminal state and @closed@ move together, the parent and
dependency graphs stay acyclic, and readiness is exactly \"open with every
dependency done\".

Store-level tests build a throwaway repository in the system temporary
directory, so they exercise real canonical files rather than an in-memory
fake.
-}
module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM, forM_)
import Data.Either (isLeft)
import Data.List (isInfixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Myque.Cli
  ( Command (..)
  , Effect (..)
  , Filters (..)
  , Transition (..)
  , emptyFilters
  , parseCommand
  , plan
  , transitionState
  )
import Myque.Frontmatter qualified as FM
import Myque.Graph
  ( blockedBy
  , childrenOf
  , dependenciesOf
  , dependencyCycles
  , edgesOf
  , isReady
  , parentCycles
  , readyItems
  )
import Myque.Item
  ( Kind (..)
  , State (..)
  , WorkItem (..)
  , decodeItem
  , encodeItem
  , itemTitle
  , keyText
  , kindText
  , newWorkItem
  , parseKey
  , parseKind
  , parseState
  , setTitle
  , stateText
  )
import Myque.Query (parseQuery, runQuery)
import Myque.Render (label, mermaidGraph)
import Myque.Store
  ( Config (..)
  , Layout (..)
  , Selector (..)
  , Store (..)
  , defaultConfig
  , discoverLayout
  , initLayout
  , itemPath
  , loadStore
  , parseConfig
  , parseSelector
  , resolveSelector
  , saveItem
  , storeItems
  )
import Myque.Timestamp (Timestamp, parseTimestamp, timestampText)
import Myque.Uuid (Uuid, isUuidV7, newUuidV7, parseUuid, uuidText, uuidVersion)
import Myque.Validate (Finding (..), validate)
import System.Directory (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import Test.Hspec hiding (Selector)

-- | A fixed instant, so encoded output is deterministic.
epoch :: Timestamp
epoch = either error id (parseTimestamp "2026-08-20T14:21:00+08:00")

-- | A later instant, for @updated@ and @closed@.
later :: Timestamp
later = either error id (parseTimestamp "2026-08-26T19:42:00+08:00")

-- | Parse a canonical ID in a test fixture.
uid :: Text -> Uuid
uid = either error id . parseUuid

-- | A minimal item with a fixed identity.
item :: Text -> Text -> WorkItem
item rawId = newWorkItem (uid rawId) Task epoch

-- | Three distinct UUIDv7 fixtures.
idA, idB, idC :: Text
idA = "019a10d8-8d48-7b77-a414-f95ab7af31be"
idB = "019a0f12-fb95-71df-8617-82e4ca982fac"
idC = "019a018c-a43e-7cd8-903d-a45e77d78865"

-- | The example item from the specification, verbatim.
specExample :: Text
specExample =
  T.unlines
    [ "---"
    , "schema: work-item/v1"
    , ""
    , "id: 019a10d8-8d48-7b77-a414-f95ab7af31be"
    , "key: C9.4"
    , ""
    , "kind: milestone"
    , "state: done"
    , ""
    , "created: 2026-08-20T14:21:00+08:00"
    , "closed: 2026-08-26T19:42:00+08:00"
    , ""
    , "tags:"
    , "  - runtime"
    , "  - lifecycle"
    , "  - sel4"
    , ""
    , "parent: 019a0f12-fb95-71df-8617-82e4ca982fac"
    , ""
    , "depends:"
    , "  - 019a018c-a43e-7cd8-903d-a45e77d78865"
    , "  - 019a09ee-7791-734e-bc6f-c95e49716892"
    , "---"
    , ""
    , "# Userspace lifecycle supervision"
    , ""
    , "## Description"
    , ""
    , "Provide userspace lifecycle supervision under declared policy."
    ]

{- | Run an action against a fresh tracker repository, cleaned up afterwards.
The action receives the loaded store, so items must be written before it
runs.
-}
withRepo :: [WorkItem] -> (Store -> IO a) -> IO a
withRepo items action = bracket create removeDirectoryRecursive $ \root -> do
  layout <- initLayout root
  forM_ items (saveItem layout)
  store <- loadStore layout
  action store
 where
  -- A fresh directory per repository: createDirectory fails rather than
  -- reusing one, so concurrent test cases cannot share state.
  create = do
    tmp <- getTemporaryDirectory
    unique <- newUuidV7
    let path = tmp </> ("myque-tracker-" <> T.unpack (uuidText unique))
    createDirectory path
    pure path

main :: IO ()
main = hspec $ do
  describe "identity" $ do
    it "generates distinct version-7 UUIDs without consulting existing ones" $ do
      uuids <- forM [1 :: Int .. 64] (const newUuidV7)
      length (sort (map uuidText uuids)) `shouldBe` 64
      sort (map uuidText uuids) `shouldBe` sort (uniqueSorted (map uuidText uuids))
      all isUuidV7 uuids `shouldBe` True
      map uuidVersion uuids `shouldBe` replicate 64 7

    it "embeds a non-decreasing millisecond timestamp" $ do
      -- Only the leading 48 bits are the timestamp; the remaining bits are
      -- entropy, so two IDs from the same millisecond have no defined order.
      stamps <- forM [1 :: Int .. 16] (const (fmap (T.take 13 . uuidText) newUuidV7))
      stamps `shouldBe` sort stamps

    it "normalises case and rejects malformed UUIDs" $ do
      fmap uuidText (parseUuid "019A10D8-8D48-7B77-A414-F95AB7AF31BE") `shouldBe` Right idA
      parseUuid "not-a-uuid" `shouldSatisfy` isLeft
      parseUuid "019a10d8-8d48-7b77-a414-f95ab7af31b" `shouldSatisfy` isLeft
      parseUuid "019a10d88d487b77a414f95ab7af31be" `shouldSatisfy` isLeft

    it "distinguishes a non-v7 UUID from a malformed one" $ do
      let v4 = "f47ac10b-58cc-4372-a567-0e02b2c3d479"
      fmap isUuidV7 (parseUuid v4) `shouldBe` Right False
      decodeItem (withField "id" v4 specExample) `shouldSatisfy` failsWith "not a UUIDv7"

  describe "frontmatter" $ do
    it "round-trips scalars that need quoting" $ do
      let awkward = ["plain", "with space", "has: colon", "trailing ", "- dash", "#hash", "quote\"d", "", "line\nbreak"]
          doc = FM.Document (FM.fromFields [("tags", FM.Sequence awkward), ("key", FM.Scalar "C9.4")]) "\nbody\n"
      fmap FM.docFrontmatter (FM.parseDocument (FM.renderDocument doc)) `shouldBe` Right (FM.docFrontmatter doc)

    it "preserves the body verbatim" $ do
      let body = "\n# Title\n\n## Notes\n\n- a\n-  b\n\n```\nliteral: not frontmatter\n```\n"
          doc = FM.Document (FM.fromFields [("key", FM.Scalar "A1")]) body
      fmap FM.docBody (FM.parseDocument (FM.renderDocument doc)) `shouldBe` Right body

    it "rejects YAML outside the supported subset" $ do
      forM_
        [ "---\nnested:\n  inner: 1\n---\n"
        , "---\nflow: [a, b]\n---\n"
        , "---\nmap: {a: 1}\n---\n"
        , "---\nanchor: &a value\n---\n"
        , "---\nblock: |\n  text\n---\n"
        ]
        (\input -> FM.parseDocument input `shouldSatisfy` isLeft)

    it "requires a frontmatter block" $ do
      FM.parseDocument "# Just markdown\n" `shouldSatisfy` isLeft
      FM.parseDocument "---\nkey: A1\n" `shouldSatisfy` isLeft

    it "reports duplicate fields" $ do
      fmap (FM.duplicateKeys . FM.docFrontmatter) (FM.parseDocument "---\nkey: A1\nkey: A2\n---\n")
        `shouldBe` Right ["key"]

  describe "item decoding" $ do
    it "decodes the specification's example" $ do
      case decodeItem specExample of
        Left err -> expectationFailure err
        Right decoded -> do
          uuidText (itemId decoded) `shouldBe` idA
          fmap keyText (itemKey decoded) `shouldBe` Just "C9.4"
          itemKind decoded `shouldBe` Milestone
          itemState decoded `shouldBe` Done
          itemTags decoded `shouldBe` ["runtime", "lifecycle", "sel4"]
          fmap uuidText (itemParent decoded) `shouldBe` Just idB
          map uuidText (itemDepends decoded) `shouldBe` [idC, "019a09ee-7791-734e-bc6f-c95e49716892"]
          fmap timestampText (itemClosed decoded) `shouldBe` Just "2026-08-26T19:42:00+08:00"
          itemTitle decoded `shouldBe` "Userspace lifecycle supervision"

    it "round-trips through encode and decode" $
      (decodeItem specExample >>= decodeItem . encodeItem) `shouldBe` decodeItem specExample

    it "rejects an unsupported frontmatter field" $
      decodeItem (addField "priority: high" specExample)
        `shouldSatisfy` failsWith "unsupported frontmatter field: priority"

    it "rejects an unknown schema version" $
      decodeItem (withField "schema" "work-item/v2" specExample)
        `shouldSatisfy` failsWith "unknown schema version"

    it "rejects missing required fields" $
      forM_ ["schema", "id", "kind", "state", "created"] $ \required ->
        decodeItem (withoutField required specExample) `shouldSatisfy` failsWith "missing required field"

    it "rejects invalid enumerations" $ do
      decodeItem (withField "kind" "chore" specExample) `shouldSatisfy` failsWith "invalid kind"
      decodeItem (withField "state" "wip" specExample) `shouldSatisfy` failsWith "invalid state"

    it "rejects timestamps without an explicit offset" $ do
      decodeItem (withField "created" "2026-08-20T14:21:00" specExample)
        `shouldSatisfy` failsWith "explicit UTC offset"
      decodeItem (withField "created" "yesterday" specExample) `shouldSatisfy` isLeft

    it "accepts Z and offset timestamps as the same instant" $
      fmap timestampText (parseTimestamp "2026-09-03T03:30:00Z")
        `shouldBe` fmap timestampText (parseTimestamp "2026-09-03T03:30:00+00:00")

    it "compares timestamps by instant, not by written offset" $
      parseTimestamp "2026-09-03T11:30:00+08:00" `shouldBe` parseTimestamp "2026-09-03T03:30:00+00:00"

    it "requires closed exactly in terminal states" $ do
      decodeItem (withoutField "closed" specExample) `shouldSatisfy` failsWith "requires 'closed'"
      decodeItem (withField "state" "open" specExample) `shouldSatisfy` failsWith "must not have 'closed'"

    it "rejects a relationship that names a key instead of an ID" $
      decodeItem (withField "parent" "C9.3" specExample) `shouldSatisfy` failsWith "field 'parent'"

    it "rejects a malformed tag" $
      decodeItem (T.replace "  - runtime" "  - two words" specExample)
        `shouldSatisfy` failsWith "must not contain whitespace"

    it "rejects a scalar where the schema requires a sequence" $
      -- A single dependency written inline must not be coerced to a
      -- one-element list; the schema declares depends as a sequence.
      decodeItem (addField ("depends: " <> idC) (withoutField "depends" specExample))
        `shouldSatisfy` failsWith "must be a sequence"

  describe "keys" $ do
    it "accepts the specification's example keys" $
      forM_ ["C9.4", "B92", "IO4", "NET-12"] $ \raw ->
        fmap keyText (parseKey raw) `shouldBe` Right raw

    it "refuses a key that is a UUID, so identity cannot be shadowed" $
      parseKey idA `shouldSatisfy` isLeft

    it "refuses empty and multi-token keys" $ do
      parseKey "" `shouldSatisfy` isLeft
      parseKey "two words" `shouldSatisfy` isLeft

    it "resolves UUIDs before keys" $ do
      parseSelector idA `shouldBe` Right (ById (uid idA))
      fmap describeSelector (parseSelector "C9.4") `shouldBe` Right "key:C9.4"

  describe "titles" $ do
    it "reads the title from the first level-1 heading" $
      itemTitle (item idA "Ethernet framing") `shouldBe` "Ethernet framing"

    it "rewrites only the title heading" $ do
      let original = (item idA "Old") {itemBody = "\n# Old\n\n## Notes\n\nkeep me\n"}
      itemBody (setTitle "New" original) `shouldBe` "\n# New\n\n## Notes\n\nkeep me\n"

    it "falls back to the key when the body has no heading" $ do
      let bare = (item idA "x") {itemBody = "no heading\n", itemKey = either (const Nothing) Just (parseKey "B92")}
      itemTitle bare `shouldBe` "B92"

  describe "storage" $ do
    it "names each file after its canonical ID" $
      withRepo [item idA "A"] $ \store -> do
        map (uuidText . itemId) (storeItems store) `shouldBe` [idA]
        storeMismatchedFiles store `shouldBe` []

    it "creates unrelated items as unrelated files" $
      withRepo [item idA "A", item idB "B"] $ \store ->
        length (storeItems store) `shouldBe` 2

    it "discovers the tracker from a subdirectory" $
      withRepo [item idA "A"] $ \store -> do
        let root = layoutRoot (storeLayout store)
            nested = root </> "src" </> "deep"
        createDirectoryIfMissing True nested
        discovered <- discoverLayout nested
        fmap layoutRoot discovered `shouldBe` Right root

    it "reports a file whose name is not its ID" $
      withRepo [item idA "A"] $ \store -> do
        let layout = storeLayout store
        TIO.writeFile (layoutRoot layout </> configItemsDir (layoutConfig layout) </> "renamed.md") (encodeItem (item idB "B"))
        reloaded <- loadStore layout
        map snd (storeMismatchedFiles reloaded) `shouldBe` [uid idB]
        validate reloaded `shouldSatisfy` any isFilenameMismatch

    it "reports a duplicate canonical ID across two files" $
      withRepo [item idA "A"] $ \store -> do
        let layout = storeLayout store
        TIO.writeFile (layoutRoot layout </> configItemsDir (layoutConfig layout) </> "copy.md") (encodeItem (item idA "A"))
        reloaded <- loadStore layout
        map fst (storeDuplicateIds reloaded) `shouldBe` [uid idA]

    it "reports an undecodable file instead of skipping it" $
      withRepo [item idA "A"] $ \store -> do
        let layout = storeLayout store
        TIO.writeFile (layoutRoot layout </> configItemsDir (layoutConfig layout) </> "broken.md") "not a work item\n"
        reloaded <- loadStore layout
        length (storeLoadErrors reloaded) `shouldBe` 1
        validate reloaded `shouldSatisfy` any isUnreadable

    it "rebuilds the same state after the derived index is discarded" $
      withRepo [item idA "A", item idB "B"] $ \store -> do
        reloaded <- loadStore (storeLayout store)
        map (uuidText . itemId) (storeItems reloaded) `shouldBe` map (uuidText . itemId) (storeItems store)

    it "survives deleting and rewriting a canonical file" $
      withRepo [item idA "A"] $ \store -> do
        let layout = storeLayout store
        removeFile (itemPath layout (uid idA))
        emptied <- loadStore layout
        storeItems emptied `shouldBe` []
        _ <- saveItem layout (item idA "A")
        restored <- loadStore layout
        map (uuidText . itemId) (storeItems restored) `shouldBe` [idA]

  describe "configuration" $ do
    it "defaults the items directory" $
      configItemsDir defaultConfig `shouldBe` ".tasks/items"

    it "parses the specification's example config" $
      fmap configItemsDir (parseConfig "schema = \"tracker-config/v1\"\n\n[storage]\nitems = \".tasks/items\"\n")
        `shouldBe` Right ".tasks/items"

    it "rejects an unknown configuration key" $
      parseConfig "[storage]\ndatabase = \"index.sqlite\"\n" `shouldSatisfy` isLeft

    it "rejects an unknown config schema" $
      parseConfig "schema = \"tracker-config/v9\"\n" `shouldSatisfy` isLeft

  describe "graph" $ do
    it "derives blocks from depends" $
      withRepo [dependsOn idA [idB], item idB "B"] $ \store -> do
        let edges = edgesOf store
        a <- resolve store idA
        b <- resolve store idB
        map uuidText (dependenciesOf edges a) `shouldBe` [idB]
        map uuidText (blockedBy edges b) `shouldBe` [idA]

    it "normalises a declared blocks edge into a dependency" $
      withRepo [item idA "A", (item idB "B") {itemBlocks = [uid idA]}] $ \store -> do
        a <- resolve store idA
        map uuidText (dependenciesOf (edgesOf store) a) `shouldBe` [idB]

    it "lists children of a parent" $
      withRepo [item idB "parent", (item idA "child") {itemParent = Just (uid idB)}] $ \store -> do
        parent <- resolve store idB
        map uuidText (childrenOf (edgesOf store) parent) `shouldBe` [idA]

    it "detects a self-parent as a cycle and a finding" $
      withRepo [(item idA "A") {itemParent = Just (uid idA)}] $ \store -> do
        parentCycles store `shouldBe` [[uid idA]]
        validate store `shouldSatisfy` elem (SelfParent (uid idA))

    it "detects a two-item parent cycle" $
      withRepo [(item idA "A") {itemParent = Just (uid idB)}, (item idB "B") {itemParent = Just (uid idA)}] $ \store ->
        parentCycles store `shouldBe` [sort [uid idA, uid idB]]

    it "detects a three-item dependency cycle" $
      withRepo [dependsOn idA [idB], dependsOn idB [idC], dependsOn idC [idA]] $ \store ->
        dependencyCycles store `shouldBe` [sort [uid idA, uid idB, uid idC]]

    it "reports no cycle for a diamond" $
      withRepo [dependsOn idA [idB, idC], dependsOn idB [idC], item idC "C"] $ \store -> do
        dependencyCycles store `shouldBe` []
        parentCycles store `shouldBe` []

    it "detects a self-dependency" $
      withRepo [dependsOn idA [idA]] $ \store -> do
        dependencyCycles store `shouldBe` [[uid idA]]
        validate store `shouldSatisfy` elem (SelfDependency (uid idA))

  describe "readiness" $ do
    it "is open with every dependency done" $
      withRepo [dependsOn idA [idB], closed idB] $ \store -> do
        a <- resolve store idA
        isReady store (edgesOf store) a `shouldBe` True

    it "is false while a dependency is unfinished" $
      withRepo [dependsOn idA [idB], item idB "B"] $ \store -> do
        a <- resolve store idA
        isReady store (edgesOf store) a `shouldBe` False

    it "is false for a non-open state, even with no dependencies" $
      withRepo [(item idA "A") {itemState = Active}] $ \store -> do
        a <- resolve store idA
        isReady store (edgesOf store) a `shouldBe` False

    it "is false when a dependency does not resolve" $
      withRepo [dependsOn idA [idB]] $ \store -> do
        a <- resolve store idA
        isReady store (edgesOf store) a `shouldBe` False

    it "lists exactly the ready items" $
      withRepo [dependsOn idA [idC], closed idC, item idB "B"] $ \store ->
        sort (map (uuidText . itemId) (readyItems store)) `shouldBe` sort [idA, idB]

    it "honours a dependency declared as the other side's blocks" $
      withRepo [item idA "A", (item idB "B") {itemBlocks = [uid idA]}] $ \store -> do
        a <- resolve store idA
        isReady store (edgesOf store) a `shouldBe` False

  describe "validation" $ do
    it "accepts a consistent repository" $
      withRepo [dependsOn idA [idB], closed idB] $ \store ->
        validate store `shouldBe` []

    it "reports every dangling reference field"
      $ withRepo
        [ (item idA "A")
            { itemParent = Just missing
            , itemDepends = [missing]
            , itemRelated = [missing]
            , itemSupersedes = [missing]
            , itemDuplicateOf = Just missing
            }
        ]
      $ \store ->
        sort (danglingFields (validate store))
          `shouldBe` sort ["parent", "depends", "related", "duplicate_of", "supersedes"]

    it "reports a duplicate key claimed by two items" $
      withRepo [keyed idA "C11", keyed idB "C11"] $ \store -> do
        map fst (storeDuplicateKeys store) `shouldBe` ["C11"]
        validate store `shouldSatisfy` any isDuplicateKey

    it "refuses to resolve an ambiguous key" $
      withRepo [keyed idA "C11", keyed idB "C11"] $ \store -> do
        keySel <- either (fail . show) pure (parseSelector "C11")
        resolveSelector store keySel `shouldSatisfy` isLeft

    it "reports timestamps that precede creation" $
      withRepo [(item idA "A") {itemUpdated = Just (either error id (parseTimestamp "2020-01-01T00:00:00+00:00"))}] $ \store ->
        validate store `shouldSatisfy` any isTimestampOrder

    it "reports a duplicate_of on an item that is still active" $
      withRepo [(item idA "A") {itemDuplicateOf = Just (uid idB), itemState = Active}, item idB "B"] $ \store ->
        validate store `shouldSatisfy` elem (DuplicateStillActive (uid idA))

  describe "readiness and queries" $ do
    it "filters by state and kind" $
      withRepo [(item idA "A") {itemKind = Milestone}, item idB "B"] $ \store -> do
        query <- either (fail . show) pure (parseQuery "state = open and kind = milestone")
        fmap (map (uuidText . itemId)) (runQuery store query) `shouldBe` Right [idA]

    it "filters by tag with a quoted literal" $
      withRepo [(item idA "A") {itemTags = ["runtime"]}, item idB "B"] $ \store -> do
        query <- either (fail . show) pure (parseQuery "tag = \"runtime\" and state != done")
        fmap (map (uuidText . itemId)) (runQuery store query) `shouldBe` Right [idA]

    it "filters by readiness" $
      withRepo [dependsOn idA [idB], item idB "B"] $ \store -> do
        query <- either (fail . show) pure (parseQuery "ready = true")
        fmap (map (uuidText . itemId)) (runQuery store query) `shouldBe` Right [idB]

    it "supports not, or and grouping" $
      withRepo [(item idA "A") {itemKind = Bug}, (item idB "B") {itemKind = Epic}, item idC "C"] $ \store -> do
        query <- either (fail . show) pure (parseQuery "not (kind = bug or kind = epic)")
        fmap (map (uuidText . itemId)) (runQuery store query) `shouldBe` Right [idC]

    it "compares timestamps chronologically" $
      withRepo [item idA "A"] $ \store -> do
        newer <- either (fail . show) pure (parseQuery "created > 2020-01-01T00:00:00+00:00")
        older <- either (fail . show) pure (parseQuery "created < 2020-01-01T00:00:00+00:00")
        fmap length (runQuery store newer) `shouldBe` Right 1
        fmap length (runQuery store older) `shouldBe` Right 0

    it "rejects an unknown property instead of matching nothing" $
      parseQuery "priority = high" `shouldSatisfy` failsWith "unknown query property"

    it "rejects a malformed expression" $ do
      parseQuery "state =" `shouldSatisfy` isLeft
      parseQuery "state open" `shouldSatisfy` isLeft
      parseQuery "(state = open" `shouldSatisfy` isLeft

  describe "command parsing" $ do
    it "parses creation options" $
      parseCommand ["new", "Userspace", "lifecycle", "supervision", "--kind", "milestone", "--key", "C9.4", "--tag", "runtime"]
        `shouldBe` Right
          ( New
              "Userspace lifecycle supervision"
              (Just Milestone)
              (either (const Nothing) Just (parseKey "C9.4"))
              ["runtime"]
              Nothing
          )

    it "parses list filters" $
      parseCommand ["list", "--state", "open", "--kind", "task", "--tag", "runtime", "--ready"]
        `shouldBe` Right (List emptyFilters {filterStates = [Open], filterKinds = [Task], filterTags = ["runtime"], filterReady = True})

    it "maps transitions onto states" $ do
      map transitionState [ToActive, ToDone, ToCancelled, ToOpen, ToDeferred, ToBlocked]
        `shouldBe` [Active, Done, Cancelled, Open, Deferred, Blocked]
      parseCommand ["close", idA] `shouldBe` Right (Transit ToDone (ById (uid idA)))

    it "rejects an unknown command, option and arity" $ do
      parseCommand ["frobnicate"] `shouldSatisfy` failsWith "unknown command"
      parseCommand ["list", "--priority", "high"] `shouldSatisfy` isLeft
      parseCommand ["show"] `shouldSatisfy` isLeft
      parseCommand ["show", idA, idB] `shouldSatisfy` isLeft
      parseCommand ["next", "extra"] `shouldSatisfy` isLeft

    it "defaults to help with no arguments" $
      parseCommand [] `shouldBe` Right Help

  describe "state transitions" $ do
    it "close sets done and a closed timestamp" $
      withRepo [item idA "A"] $ \store ->
        case plan store later (Transit ToDone (ById (uid idA))) of
          Right (Written [updated]) -> do
            itemState updated `shouldBe` Done
            itemClosed updated `shouldBe` Just later
            itemUpdated updated `shouldBe` Just later
          other -> expectationFailure (show' other)

    it "reopen clears the closed timestamp" $
      withRepo [closed idA] $ \store ->
        case plan store later (Transit ToOpen (ById (uid idA))) of
          Right (Written [updated]) -> do
            itemState updated `shouldBe` Open
            itemClosed updated `shouldBe` Nothing
          other -> expectationFailure (show' other)

    it "keeps created immutable across a transition" $
      withRepo [item idA "A"] $ \store ->
        case plan store later (Transit ToActive (ById (uid idA))) of
          Right (Written [updated]) -> itemCreated updated `shouldBe` epoch
          other -> expectationFailure (show' other)

    it "rejects a transition that would change nothing" $
      withRepo [(item idA "A") {itemState = Active}] $ \store ->
        plan store later (Transit ToActive (ById (uid idA))) `shouldSatisfy` isLeft

    it "cancel via duplicate records the canonical item and closes it" $
      withRepo [item idA "A", item idB "B"] $ \store ->
        case plan store later (Duplicate (ById (uid idA)) (ById (uid idB))) of
          Right (Written [updated]) -> do
            itemState updated `shouldBe` Cancelled
            itemDuplicateOf updated `shouldBe` Just (uid idB)
            itemClosed updated `shouldBe` Just later
          other -> expectationFailure (show' other)

  describe "relationship commands" $ do
    it "persists a dependency added by key as a UUID" $
      withRepo [item idA "A", keyed idB "C9.3"] $ \store -> do
        sel <- either (fail . show) pure (parseSelector "C9.3")
        case plan store later (Depend True (ById (uid idA)) sel) of
          Right (Written [updated]) -> itemDepends updated `shouldBe` [uid idB]
          other -> expectationFailure (show' other)

    it "refuses a dependency that would close a cycle" $
      withRepo [dependsOn idB [idA], item idA "A"] $ \store ->
        plan store later (Depend True (ById (uid idA)) (ById (uid idB))) `shouldSatisfy` isLeft

    it "refuses a self dependency, self parent and self relation" $
      withRepo [item idA "A"] $ \store -> do
        let self = ById (uid idA)
        plan store later (Depend True self self) `shouldSatisfy` isLeft
        plan store later (Parent self (Just self)) `shouldSatisfy` isLeft
        plan store later (Relate True self self) `shouldSatisfy` isLeft

    it "refuses a parent that is already a descendant" $
      withRepo [item idA "A", (item idB "B") {itemParent = Just (uid idA)}] $ \store ->
        plan store later (Parent (ById (uid idA)) (Just (ById (uid idB)))) `shouldSatisfy` isLeft

    it "records relate on both items" $
      withRepo [item idA "A", item idB "B"] $ \store ->
        case plan store later (Relate True (ById (uid idA)) (ById (uid idB))) of
          Right (Written [first, second]) -> do
            itemRelated first `shouldBe` [uid idB]
            itemRelated second `shouldBe` [uid idA]
          other -> expectationFailure (show' other)

    it "removes a dependency without touching others" $
      withRepo [dependsOn idA [idB, idC], item idB "B", item idC "C"] $ \store ->
        case plan store later (Depend False (ById (uid idA)) (ById (uid idB))) of
          Right (Written [updated]) -> itemDepends updated `shouldBe` [uid idC]
          other -> expectationFailure (show' other)

    it "clears a parent" $
      withRepo [(item idA "A") {itemParent = Just (uid idB)}, item idB "B"] $ \store ->
        case plan store later (Parent (ById (uid idA)) Nothing) of
          Right (Written [updated]) -> itemParent updated `shouldBe` Nothing
          other -> expectationFailure (show' other)

    it "refuses a key already claimed by another item" $
      withRepo [item idA "A", keyed idB "C9.3"] $ \store ->
        plan store later (SetKey (ById (uid idA)) "C9.3") `shouldSatisfy` isLeft

    it "allows an item to keep its own key" $
      withRepo [keyed idA "C9.3"] $ \store ->
        plan store later (SetKey (ById (uid idA)) "C9.3") `shouldSatisfy` isRight'

    it "supersede records the replaced item without changing its state" $
      withRepo [item idA "new", item idB "old"] $ \store ->
        case plan store later (Supersede (ById (uid idA)) (ById (uid idB))) of
          Right (Written [updated]) -> do
            itemSupersedes updated `shouldBe` [uid idB]
            length (storeItems store) `shouldBe` 2
          other -> expectationFailure (show' other)

    it "rm deletes exactly one item" $
      withRepo [item idA "A", item idB "B"] $ \store ->
        case plan store later (Remove (ById (uid idA))) of
          Right (Deleted target) -> itemId target `shouldBe` uid idA
          other -> expectationFailure (show' other)

  describe "rendering" $ do
    it "labels an item by key when it has one" $
      label (keyed idA "C9.4") `shouldBe` "C9.4"

    it "labels an item by a short ID otherwise" $
      label (item idA "A") `shouldBe` "019a10d8"

    it "emits a Mermaid node per item and an edge per relationship" $
      withRepo [dependsOn idA [idB], (item idB "B") {itemParent = Just (uid idC)}, item idC "C"] $ \store -> do
        let rendered = T.unpack (mermaidGraph store)
        rendered `shouldSatisfy` isInfixOf "flowchart TD"
        rendered `shouldSatisfy` isInfixOf ("n" <> filter (/= '-') (T.unpack idA))
        length (filter ("-->" `isInfixOf`) (lines rendered)) `shouldBe` 1
        length (filter ("-.->" `isInfixOf`) (lines rendered)) `shouldBe` 1

    it "keeps generated views out of canonical storage" $
      withRepo [item idA "A"] $ \store -> do
        _ <- pure (mermaidGraph store)
        reloaded <- loadStore (storeLayout store)
        length (storeItems reloaded) `shouldBe` 1

  describe "enumerations" $
    it "round-trips every kind and state through its wire form" $ do
      forM_ [Task, Issue, Bug, Milestone, Epic, Followup] $ \k ->
        parseKind (kindText k) `shouldBe` Right k
      forM_ [Open, Active, Blocked, Deferred, Done, Cancelled] $ \s ->
        parseState (stateText s) `shouldBe` Right s

-- | An item that depends on the given IDs.
dependsOn :: Text -> [Text] -> WorkItem
dependsOn self deps = (item self "dependent") {itemDepends = map uid deps}

-- | A @done@ item with a @closed@ timestamp.
closed :: Text -> WorkItem
closed self = (item self "closed") {itemState = Done, itemClosed = Just later}

-- | An item carrying a key.
keyed :: Text -> Text -> WorkItem
keyed self key = (item self "keyed") {itemKey = either (const Nothing) Just (parseKey key)}

-- | A missing reference used in dangling-reference tests.
missing :: Uuid
missing = uid "019a09ee-7791-734e-bc6f-c95e49716892"

-- | Resolve an ID against a store, failing the test if it is absent.
resolve :: Store -> Text -> IO WorkItem
resolve store raw = either fail pure (resolveSelector store (ById (uid raw)))

-- | Whether an 'Either' failure mentions a substring.
failsWith :: String -> Either String a -> Bool
failsWith needle (Left err) = needle `isInfixOf` err
failsWith _ _ = False

-- | Whether a result succeeded.
isRight' :: Either a b -> Bool
isRight' = either (const False) (const True)

-- | Deduplicate a sorted list.
uniqueSorted :: (Eq a) => [a] -> [a]
uniqueSorted (a : b : rest) | a == b = uniqueSorted (b : rest)
uniqueSorted (a : rest) = a : uniqueSorted rest
uniqueSorted [] = []

-- | Describe a selector for assertions.
describeSelector :: Selector -> Text
describeSelector (ById u) = "id:" <> uuidText u
describeSelector (ByKey k) = "key:" <> keyText k

-- | The relationship fields named by dangling-reference findings.
danglingFields :: [Finding] -> [Text]
danglingFields findings = [fld | DanglingReference _ fld _ <- findings]

-- | Whether a finding is a filename mismatch.
isFilenameMismatch :: Finding -> Bool
isFilenameMismatch FilenameMismatch {} = True
isFilenameMismatch _ = False

-- | Whether a finding is an undecodable file.
isUnreadable :: Finding -> Bool
isUnreadable FileUnreadable {} = True
isUnreadable _ = False

-- | Whether a finding is a duplicate key.
isDuplicateKey :: Finding -> Bool
isDuplicateKey DuplicateKey {} = True
isDuplicateKey _ = False

-- | Whether a finding is an out-of-order timestamp.
isTimestampOrder :: Finding -> Bool
isTimestampOrder TimestampOutOfOrder {} = True
isTimestampOrder _ = False

-- | Render an unexpected plan result for a failure message.
show' :: Either String Effect -> String
show' = show

-- | Replace a scalar field's value in a fixture.
withField :: Text -> Text -> Text -> Text
withField name value = T.unlines . map replace . T.lines
 where
  replace line
    | T.isPrefixOf (name <> ":") line = name <> ": " <> value
    | otherwise = line

-- | Drop a field, and any sequence items under it, from a fixture.
withoutField :: Text -> Text -> Text
withoutField name = T.unlines . go . T.lines
 where
  go [] = []
  go (line : rest)
    | T.isPrefixOf (name <> ":") line = go (dropWhile isItem rest)
    | otherwise = line : go rest
  isItem = T.isPrefixOf "  -"

-- | Insert an extra frontmatter line into a fixture.
addField :: Text -> Text -> Text
addField line = T.replace "schema: work-item/v1" ("schema: work-item/v1\n" <> line)
