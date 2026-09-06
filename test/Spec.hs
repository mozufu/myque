{-# LANGUAGE LambdaCase #-}
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
import Data.List (isInfixOf, isSuffixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Myque.Cli
  ( Command (..)
  , Effect (..)
  , Failure (..)
  , Filters (..)
  , Format (..)
  , Transition (..)
  , commandUsage
  , emptyFilters
  , failureStatus
  , parseCommand
  , parseFormat
  , plan
  , runCommand
  , transitionState
  )
import Myque.Frontmatter qualified as FM
import Myque.Graph
  ( Cycle (..)
  , blockedBy
  , childrenOf
  , cycleWitnessLimit
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
  , parseTag
  , setTitle
  , stateText
  )
import Myque.Query (parseQuery, runQuery)
import Myque.Render (abbreviate, idLines, jsonLines, label, mermaidGraph)
import Myque.Store
  ( Config (..)
  , Layout (..)
  , Selector (..)
  , Store (..)
  , defaultConfig
  , discoverLayout
  , initLayout
  , invalidFiles
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
import Myque.Validate (Finding (..), findingText, validate)
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  , withCurrentDirectory
  )
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

{- | Two IDs allocated in the same millisecond, so they agree through the
timestamp field and differ only in entropy. This is what an import that
backdates items to one historical closure date produces.
-}
sameMs1, sameMs2 :: Text
sameMs1 = "019a10d8-8d48-7c01-b3f2-0e6c1a9d4471"
sameMs2 = "019a10d8-8d48-7c01-9a04-52b7e8f0cd33"

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
withRepo items action = withScratch "tracker" $ \root -> do
  layout <- initLayout root
  forM_ items (saveItem layout)
  store <- loadStore layout
  action store

{- | Run an action in a directory with no tracker in it or in any parent,
for the \"no tracker found\" path.
-}
withBareDirectory :: (FilePath -> IO a) -> IO a
withBareDirectory = withScratch "bare"

{- | A fresh temporary directory per case, removed afterwards.
'createDirectory' fails rather than reusing one, so concurrent cases cannot
share state.
-}
withScratch :: String -> (FilePath -> IO a) -> IO a
withScratch name = bracket create removeDirectoryRecursive
 where
  create = do
    tmp <- getTemporaryDirectory
    unique <- newUuidV7
    let path = tmp </> ("myque-" <> name <> "-" <> T.unpack (uuidText unique))
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

  describe "selectors" $ do
    it "accepts the abbreviation the tables print" $
      -- Rendering abbreviates against the store, so show must consume
      -- exactly what was displayed.
      withRepo [item idA "A"] $ \store -> do
        sel <- either fail pure (parseSelector (label (abbreviate store) (item idA "A")))
        fmap (uuidText . itemId) (resolveSelector store sel) `shouldBe` Right idA

    it "reads an abbreviation as an id, never as a key" $ do
      parseSelector "019a10d8" `shouldBe` Right (ByPrefix "019a10d8")
      parseSelector "019a10d8-8d48" `shouldBe` Right (ByPrefix "019a10d8-8d48")
      parseSelector "C9.4" `shouldSatisfy` (/= Right (ByPrefix "C9.4"))

    it "matches an abbreviation case-insensitively, as UUID parsing does" $
      withRepo [item idA "A"] $ \store -> do
        sel <- either fail pure (parseSelector "019A10D8")
        fmap (uuidText . itemId) (resolveSelector store sel) `shouldBe` Right idA

    it "prefers an id abbreviation over a key that spells the same text" $
      -- Specification §17.2: id resolution takes precedence, so a key that
      -- happens to look like an abbreviation must not shadow the item.
      withRepo [item idA "A", keyed idB "019a10d8"] $ \store -> do
        sel <- either fail pure (parseSelector "019a10d8")
        fmap (uuidText . itemId) (resolveSelector store sel) `shouldBe` Right idA

    it "falls back to a key when no id has that prefix" $
      withRepo [item idA "A", keyed idB "abcdef"] $ \store -> do
        sel <- either fail pure (parseSelector "abcdef")
        fmap (uuidText . itemId) (resolveSelector store sel) `shouldBe` Right idB

    it "keeps a hex-shaped key's case when falling back to it" $
      -- 'D1' is a valid abbreviation shape, so the id attempt must not fold
      -- the text before the key lookup or the key becomes unaddressable.
      withRepo [keyed idA "D1", keyed idB "IO4"] $ \store -> do
        d1 <- either fail pure (parseSelector "D1")
        io4 <- either fail pure (parseSelector "IO4")
        fmap (uuidText . itemId) (resolveSelector store d1) `shouldBe` Right idA
        fmap (uuidText . itemId) (resolveSelector store io4) `shouldBe` Right idB

    it "fails closed on an ambiguous abbreviation, naming every candidate" $ do
      let twin = "019a10d8-0000-7000-8000-000000000000"
      withRepo [item idA "A", item twin "twin"] $ \store -> do
        sel <- either fail pure (parseSelector "019a10d8")
        resolveSelector store sel `shouldSatisfy` failsWith "ambiguous"
        resolveSelector store sel `shouldSatisfy` failsWith (T.unpack idA)
        resolveSelector store sel `shouldSatisfy` failsWith (T.unpack twin)

    it "reports an unmatched abbreviation as absent" $
      withRepo [item idA "A"] $ \store -> do
        sel <- either fail pure (parseSelector "deadbeef")
        resolveSelector store sel `shouldSatisfy` failsWith "no work item with an id starting deadbeef"

    it "says an item exists but is invalid instead of saying it is absent" $
      -- loadStore drops a malformed item from the index, which is right for
      -- check but would otherwise make every mutating command claim the one
      -- file that needs fixing does not exist.
      withRepo [item idB "B"] $ \store -> do
        let layout = storeLayout store
        TIO.writeFile (itemPath layout (uid idA)) (T.replace "tags:\n" "tags:\n  - two words\n" (encodeItem ((item idA "A") {itemTags = ["ok"]})))
        reloaded <- loadStore layout
        map fst (invalidFiles reloaded) `shouldBe` [uid idA]
        resolveSelector reloaded (ById (uid idA)) `shouldSatisfy` failsWith "exists but is invalid"
        resolveSelector reloaded (ById (uid idA)) `shouldSatisfy` failsWith "two words"
        prefix <- either fail pure (parseSelector "019a10d8")
        resolveSelector reloaded prefix `shouldSatisfy` failsWith "exists but is invalid"

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
        map cycleWitness (parentCycles store) `shouldBe` [[uid idA]]
        map cycleClosing (parentCycles store) `shouldBe` [(uid idA, uid idA)]
        validate store `shouldSatisfy` any isParentCycle

    it "detects a two-item parent cycle" $
      withRepo [(item idA "A") {itemParent = Just (uid idB)}, (item idB "B") {itemParent = Just (uid idA)}] $ \store ->
        map (sort . cycleWitness) (parentCycles store) `shouldBe` [sort [uid idA, uid idB]]

    it "detects a three-item dependency cycle" $
      withRepo [dependsOn idA [idB], dependsOn idB [idC], dependsOn idC [idA]] $ \store -> do
        map (sort . cycleWitness) (dependencyCycles store) `shouldBe` [sort [uid idA, uid idB, uid idC]]
        map cycleLength (dependencyCycles store) `shouldBe` [3]

    it "names the closing edge of a cycle" $
      -- The witness starts at the lowest ID, so the closing edge runs from
      -- the cycle's last member back to it.
      withRepo [dependsOn idA [idB], dependsOn idB [idA]] $ \store ->
        case dependencyCycles store of
          [c] -> do
            let (lower, higher) = (min (uid idA) (uid idB), max (uid idA) (uid idB))
            take 1 (cycleWitness c) `shouldBe` [lower]
            cycleClosing c `shouldBe` (higher, lower)
          other -> expectationFailure ("expected one cycle, got " <> show (length other))

    it "bounds a cycle witness and its rendered finding regardless of graph size" $ do
      -- A long chain with one back edge is the expected shape of a real
      -- roadmap graph, so neither the witness nor the finding may grow with it.
      ids <- forM [1 :: Int .. 200] (const newUuidV7)
      let chain = zipWith (\self next -> (item (uuidText self) "n") {itemDepends = [next]}) ids (drop 1 ids <> take 1 ids)
      withRepo chain $ \store ->
        case dependencyCycles store of
          [c] -> do
            cycleLength c `shouldBe` 200
            length (cycleWitness c) `shouldBe` cycleWitnessLimit
            forM_ (validate store) $ \finding ->
              T.length (findingText finding) `shouldSatisfy` (< 300)
          other -> expectationFailure ("expected one cycle, got " <> show (length other))

    it "reports no cycle for a diamond" $
      withRepo [dependsOn idA [idB, idC], dependsOn idB [idC], item idC "C"] $ \store -> do
        dependencyCycles store `shouldBe` []
        parentCycles store `shouldBe` []

    it "detects a self-dependency" $
      withRepo [dependsOn idA [idA]] $ \store -> do
        map cycleWitness (dependencyCycles store) `shouldBe` [[uid idA]]
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
        `shouldBe` Right
          ( List
              emptyFilters {filterStates = [Open], filterKinds = [Task], filterTags = ["runtime"], filterReady = True}
              Table
          )

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
      parseCommand [] `shouldBe` Right (Help Nothing)

    it "names an unknown option rather than reporting a missing value" $ do
      -- --help is not a 'new' option, and reporting it as a missing value
      -- hides both facts: that it is unknown and what is accepted.
      parseCommand ["new", "x", "--priority", "high"] `shouldSatisfy` failsWith "unknown option for 'new': --priority"
      parseCommand ["new", "x", "--priority", "high"] `shouldSatisfy` failsWith "--kind <kind>"
      parseCommand ["list", "--priority"] `shouldSatisfy` failsWith "unknown option for 'list': --priority"

    it "reports a known option that is missing its value as such" $
      parseCommand ["new", "x", "--kind"] `shouldSatisfy` failsWith "missing value for --kind"

    it "prints per-command help for a command's own options" $ do
      parseCommand ["new", "--help"] `shouldBe` Right (Help (Just "new"))
      parseCommand ["help", "list"] `shouldBe` Right (Help (Just "list"))
      parseCommand ["help", "frobnicate"] `shouldSatisfy` failsWith "unknown command"
      commandUsage "list" `shouldSatisfy` isInfixOf "--ready"
      commandUsage "key" `shouldSatisfy` isInfixOf "--unset"

    it "parses every output format and rejects an unknown one" $ do
      map parseFormat ["table", "id", "json"] `shouldBe` [Right Table, Right Ids, Right Json]
      parseFormat "yaml" `shouldSatisfy` failsWith "invalid format: yaml"
      parseCommand ["list", "--format", "yaml"] `shouldSatisfy` failsWith "invalid format"
      parseCommand ["list", "--format", "id"] `shouldBe` Right (List emptyFilters Ids)
      parseCommand ["next", "--format", "json"] `shouldBe` Right (Next Json)
      parseCommand ["query", "state", "=", "open", "--format", "id"]
        `shouldBe` Right (Query "state = open" Ids)

    it "parses a key assignment and a key removal" $ do
      parseCommand ["key", idA, "C9.4"] `shouldBe` Right (SetKey (ById (uid idA)) (Just "C9.4"))
      parseCommand ["key", idA, "--unset"] `shouldBe` Right (SetKey (ById (uid idA)) Nothing)
      parseCommand ["key", idA] `shouldSatisfy` isLeft

    it "rejects a malformed tag before it can be written" $ do
      -- The store refuses to load an item with a whitespace tag, so a write
      -- command that accepted one would produce a file the CLI cannot repair.
      parseCommand ["new", "x", "--tag", "two words"] `shouldSatisfy` failsWith "must not contain whitespace"
      parseCommand ["tag", idA, "two words"] `shouldSatisfy` failsWith "must not contain whitespace"
      parseCommand ["list", "--tag", "two words"] `shouldSatisfy` failsWith "must not contain whitespace"
      parseTag "runtime" `shouldBe` Right "runtime"

    it "still accepts a malformed tag for removal" $
      -- Rejecting it here would leave a tag that only a hand edit can undo.
      parseCommand ["untag", idA, "two words"]
        `shouldBe` Right (Tag False (ById (uid idA)) ["two words"])

    it "parses the version request" $ do
      parseCommand ["--version"] `shouldBe` Right Version
      parseCommand ["version"] `shouldBe` Right Version

  describe "exit status" $ do
    it "separates findings, usage failures and rejected commands" $
      -- A CI gate has to distinguish "the tracker is invalid" from "there is
      -- no tracker here", so the two must never share an exit status.
      map failureStatus [Findings "x", Usage "y", Rejected "z"] `shouldBe` [1, 2, 3]

    it "exits 0 with a summary over a clean store" $
      withRepo [item idA "A"] $ \store ->
        withCurrentDirectory (layoutRoot (storeLayout store)) $
          runCommand Check `shouldReturn` Right "checked 1 work item(s): no findings\n"

    it "reports findings as the command's result, not as a diagnostic" $
      withRepo [(item idA "A") {itemParent = Just missing}] $ \store ->
        withCurrentDirectory (layoutRoot (storeLayout store)) $
          runCommand Check >>= \case
            Left failure@(Findings findings) -> do
              failureStatus failure `shouldBe` 1
              T.unpack findings `shouldSatisfy` isInfixOf "1 finding(s)"
              T.unpack findings `shouldSatisfy` isInfixOf "dangling parent reference"
            other -> expectationFailure ("expected findings, got " <> show other)

    it "distinguishes a missing tracker from an invalid one" $
      -- Tracker discovery ascends, so the probe directory must have no
      -- .tasks in it or in any parent.
      withBareDirectory $ \bare -> do
        result <- withCurrentDirectory bare (runCommand Check)
        case result of
          Left failure@(Usage err) -> do
            failureStatus failure `shouldBe` 2
            err `shouldSatisfy` isInfixOf "no tracker found"
          other -> expectationFailure ("expected a usage failure, got " <> show other)

    it "reports an unresolvable item as a rejected command, not a usage error" $
      withRepo [item idA "A"] $ \store ->
        withCurrentDirectory (layoutRoot (storeLayout store)) $
          runCommand (Show (ById (uid idB))) >>= \case
            Left failure@(Rejected err) -> do
              failureStatus failure `shouldBe` 3
              err `shouldSatisfy` isInfixOf "no work item with id"
            other -> expectationFailure ("expected a rejected command, got " <> show other)

    it "prints the package version" $
      runCommand Version >>= \case
        Right rendered -> do
          T.unpack (T.strip rendered) `shouldSatisfy` isInfixOf "0.1.0.0"
          T.unpack rendered `shouldSatisfy` isSuffixOf "\n"
        other -> expectationFailure ("expected a version, got " <> show other)

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
        plan store later (SetKey (ById (uid idA)) (Just "C9.3")) `shouldSatisfy` isLeft

    it "allows an item to keep its own key" $
      withRepo [keyed idA "C9.3"] $ \store ->
        plan store later (SetKey (ById (uid idA)) (Just "C9.3")) `shouldSatisfy` isRight'

    it "unsets a key without disturbing relationships" $
      -- Relationships persist UUIDs, so dropping the display alias must
      -- leave every edge byte-identical and the store valid.
      withRepo [(keyed idA "C9.3") {itemDepends = [uid idB], itemParent = Just (uid idC)}, item idB "B", item idC "C"] $ \store ->
        case plan store later (SetKey (ById (uid idA)) Nothing) of
          Right (Written [updated]) -> do
            itemKey updated `shouldBe` Nothing
            itemDepends updated `shouldBe` [uid idB]
            itemParent updated `shouldBe` Just (uid idC)
            itemUpdated updated `shouldBe` Just later
            _ <- saveItem (storeLayout store) updated
            reloaded <- loadStore (storeLayout store)
            validate reloaded `shouldBe` []
          other -> expectationFailure (show' other)

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
      withRepo [keyed idA "C9.4"] $ \store ->
        label (abbreviate store) (keyed idA "C9.4") `shouldBe` "C9.4"

    it "labels an item by an abbreviated ID otherwise" $
      withRepo [item idA "A"] $ \store ->
        label (abbreviate store) (item idA "A") `shouldBe` "019a10d8"

    it "widens an abbreviation until it names exactly one item" $
      -- UUIDv7's leading digits are its millisecond timestamp, so items
      -- created in the same minute -- or backdated to one historical day by
      -- an import -- share the whole first group. A fixed width would print
      -- one repeated token for all of them.
      withRepo [item sameMs1 "A", item sameMs2 "B", item idC "C"] $ \store -> do
        let abbrev = abbreviate store
            one = label abbrev (item sameMs1 "A")
            two = label abbrev (item sameMs2 "B")
        one `shouldSatisfy` (/= two)
        T.take 8 one `shouldBe` T.take 8 two
        label abbrev (item idC "C") `shouldBe` "019a018c"

    it "prints only abbreviations that resolve back to one item" $
      -- Specification §17.2: displaying an identifier the tool cannot
      -- consume is a defect, so every rendered label must round-trip.
      withRepo [item sameMs1 "A", item sameMs2 "B", keyed idC "IO4"] $ \store ->
        forM_ (storeItems store) $ \it -> do
          sel <- either fail pure (parseSelector (label (abbreviate store) it))
          fmap itemId (resolveSelector store sel) `shouldBe` Right (itemId it)

    it "never abbreviates onto a group separator" $
      -- A prefix ending in '-' is not a well-formed selector, so widening
      -- has to step over the separator positions.
      withRepo [item sameMs1 "A", item sameMs2 "B"] $ \store ->
        forM_ (storeItems store) $ \it ->
          T.last (label (abbreviate store) it) `shouldSatisfy` (/= '-')

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

    it "enumerates canonical ids in full, one per line" $
      -- The KEY column is deliberately abbreviated, so a script needs a
      -- form that emits identities it can feed straight back in.
      withRepo [item idA "A", item idB "B"] $ \store -> do
        let rendered = idLines (storeItems store)
        sort (T.lines rendered) `shouldBe` sort [idA, idB]
        forM_ (T.lines rendered) $ \line -> T.length line `shouldBe` 36

    it "renders an empty selection as no output at all" $
      withRepo [] $ \store -> do
        idLines (storeItems store) `shouldBe` ""
        jsonLines store (storeItems store) `shouldBe` ""

    it "emits one JSON object per item with full ids in every relationship" $
      withRepo [(keyed idA "C9.4") {itemDepends = [uid idB], itemParent = Just (uid idC)}, closed idB, item idC "C"] $ \store -> do
        a <- resolve store idA
        case T.lines (jsonLines store [a]) of
          [line] -> do
            let rendered = T.unpack line
            rendered `shouldSatisfy` isInfixOf ("\"id\":\"" <> T.unpack idA <> "\"")
            rendered `shouldSatisfy` isInfixOf "\"key\":\"C9.4\""
            rendered `shouldSatisfy` isInfixOf ("\"depends\":[\"" <> T.unpack idB <> "\"]")
            rendered `shouldSatisfy` isInfixOf ("\"parent\":\"" <> T.unpack idC <> "\"")
            rendered `shouldSatisfy` isInfixOf "\"ready\":true"
            rendered `shouldSatisfy` isInfixOf "\"closed\":null"
          other -> expectationFailure ("expected one line, got " <> show (length other))

    it "escapes a title that would otherwise break the JSON line" $
      withRepo [(item idA "A") {itemBody = "\n# a \"quoted\"\\ title\ttabbed\n"}] $ \store -> do
        a <- resolve store idA
        let rendered = jsonLines store [a]
        T.length (T.filter (== '\n') rendered) `shouldBe` 1
        T.unpack rendered `shouldSatisfy` isInfixOf "a \\\"quoted\\\"\\\\ title\\ttabbed"

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
describeSelector (ByPrefix p) = "prefix:" <> p
describeSelector (ByKey k) = "key:" <> keyText k

-- | The relationship fields named by dangling-reference findings.
danglingFields :: [Finding] -> [Text]
danglingFields findings = [fld | DanglingReference _ fld _ <- findings]

-- | Whether a finding is a filename mismatch.
isFilenameMismatch :: Finding -> Bool
isFilenameMismatch FilenameMismatch {} = True
isFilenameMismatch _ = False

-- | Whether a finding is a parent cycle.
isParentCycle :: Finding -> Bool
isParentCycle ParentCycle {} = True
isParentCycle _ = False

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
