{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The @myque@ command-line interface.

Arguments are parsed into a 'Command' first and executed second, and every
mutation is computed as a pure 'Effect' by 'plan' before any file is touched.
That keeps argument handling and mutation semantics testable without a
repository, and means a rejected command writes nothing.

Mutations rewrite exactly the affected items and set @updated@. Files that
fail to decode are reported on stderr rather than silently skipped, so a
malformed item can never quietly disappear from @list@ output.

Exit status is @0@ on success and @1@ on a usage error, an execution error,
or a @check@ that produced findings.
-}
module Myque.Cli
  ( Command (..)
  , Filters (..)
  , emptyFilters
  , Transition (..)
  , transitionState
  , parseCommand
  , runCommand
  , plan
  , Effect (..)
  , usage
  , main
  ) where

import Control.Monad (forM_, unless, when)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Myque.Graph (dependenciesOf, descendantsOf, edgesOf, isReady, readyItems)
import Myque.Item
  ( Key
  , Kind (..)
  , State (..)
  , WorkItem (..)
  , allKinds
  , allStates
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
import Myque.Render (itemDetail, itemRows, label, markdownSummary, mermaidGraph)
import Myque.Store
  ( Layout (..)
  , Selector
  , Store (..)
  , deleteItem
  , discoverLayout
  , formatLoadError
  , initLayout
  , itemPath
  , loadStore
  , parseSelector
  , resolveSelector
  , saveItem
  , storeItems
  )
import Myque.Timestamp (Timestamp, currentTimestamp)
import Myque.Uuid (Uuid, newUuidV7, uuidText)
import Myque.Validate (findingText, validate)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (makeRelative)
import System.IO (hPutStrLn, stderr)

-- | A parsed invocation.
data Command
  = -- | @myque init@
    Init
  | -- | @myque new \<title\> [--kind --key --tag --parent]@
    New Text (Maybe Kind) (Maybe Key) [Text] (Maybe Selector)
  | -- | @myque show \<item\>@
    Show Selector
  | -- | @myque list [--state --kind --tag --parent --ready]@
    List Filters
  | -- | @myque next@
    Next
  | -- | @myque check@
    Check
  | -- | @myque start|close|cancel|reopen|defer|block \<item\>@
    Transit Transition Selector
  | -- | @myque key \<item\> \<key\>@
    SetKey Selector Text
  | -- | @myque title \<item\> \<title\>@
    SetTitle Selector Text
  | -- | @myque tag@ when 'True', @myque untag@ when 'False'
    Tag Bool Selector [Text]
  | -- | @myque depend@ when 'True', @myque undepend@ when 'False'
    Depend Bool Selector Selector
  | -- | @myque parent \<item\> [\<parent\>]@; 'Nothing' clears it
    Parent Selector (Maybe Selector)
  | -- | @myque relate@ when 'True', @myque unrelate@ when 'False'
    Relate Bool Selector Selector
  | -- | @myque duplicate \<item\> \<canonical\>@
    Duplicate Selector Selector
  | -- | @myque supersede \<new\> \<old\>@
    Supersede Selector Selector
  | -- | @myque rm \<item\>@
    Remove Selector
  | -- | @myque query \<expression\>@
    Query Text
  | -- | @myque graph@
    Graph
  | -- | @myque render@
    Render
  | -- | @myque help@
    Help
  deriving (Eq, Show)

-- | @list@ filters. Every supplied filter must hold.
data Filters = Filters
  { filterStates :: [State]
  , filterKinds :: [Kind]
  , filterTags :: [Text]
  , filterParent :: Maybe Selector
  , filterReady :: Bool
  }
  deriving (Eq, Show)

-- | No filtering: every item.
emptyFilters :: Filters
emptyFilters = Filters [] [] [] Nothing False

-- | A state-transition command.
data Transition = ToActive | ToDone | ToCancelled | ToOpen | ToDeferred | ToBlocked
  deriving (Eq, Show)

-- | The state a transition moves an item into.
transitionState :: Transition -> State
transitionState = \case
  ToActive -> Active
  ToDone -> Done
  ToCancelled -> Cancelled
  ToOpen -> Open
  ToDeferred -> Deferred
  ToBlocked -> Blocked

-- | Parse command-line arguments.
parseCommand :: [Text] -> Either String Command
parseCommand [] = Right Help
parseCommand (verb : rest) = case verb of
  "init" -> noArgs Init
  "new" -> parseNew rest
  "show" -> Show <$> oneSelector
  "list" -> List <$> parseFilters emptyFilters rest
  "next" -> noArgs Next
  "check" -> noArgs Check
  "start" -> transit ToActive
  "close" -> transit ToDone
  "cancel" -> transit ToCancelled
  "reopen" -> transit ToOpen
  "defer" -> transit ToDeferred
  "block" -> transit ToBlocked
  "key" -> two (\a b -> SetKey <$> parseSelector a <*> pure b)
  "title" -> case rest of
    target : titleWords
      | not (null titleWords) -> SetTitle <$> parseSelector target <*> pure (T.unwords titleWords)
    _ -> Left "usage: myque title <item> <title>"
  "tag" -> tags True
  "untag" -> tags False
  "depend" -> two (relation (Depend True))
  "undepend" -> two (relation (Depend False))
  "parent" -> case rest of
    [target] -> Parent <$> parseSelector target <*> pure Nothing
    [target, parent] -> Parent <$> parseSelector target <*> (Just <$> parseSelector parent)
    _ -> Left "usage: myque parent <item> [<parent>]   (omit <parent> to clear it)"
  "relate" -> two (relation (Relate True))
  "unrelate" -> two (relation (Relate False))
  "duplicate" -> two (relation Duplicate)
  "supersede" -> two (relation Supersede)
  "rm" -> Remove <$> oneSelector
  "query"
    | null rest -> Left "usage: myque query <expression>"
    | otherwise -> Right (Query (T.unwords rest))
  "graph" -> noArgs Graph
  "render" -> noArgs Render
  _
    | verb `elem` ["help", "--help", "-h"] -> Right Help
    | otherwise -> Left ("unknown command: " <> T.unpack verb <> "\n\n" <> usage)
 where
  transit t = Transit t <$> oneSelector

  noArgs cmd
    | null rest = Right cmd
    | otherwise = Left ("myque " <> T.unpack verb <> " takes no arguments")

  oneSelector = case rest of
    [target] -> parseSelector target
    _ -> Left ("usage: myque " <> T.unpack verb <> " <item>")

  two build = case rest of
    [a, b] -> build a b
    _ -> Left ("usage: myque " <> T.unpack verb <> " <item> <item>")

  relation build a b = build <$> parseSelector a <*> parseSelector b

  tags adding = case rest of
    target : ts | not (null ts) -> Tag adding <$> parseSelector target <*> pure (nub ts)
    _ -> Left ("usage: myque " <> T.unpack verb <> " <item> <tag>...")

-- | Parse @new@: title words up to the first option, then the options.
parseNew :: [Text] -> Either String Command
parseNew args
  | null titleWords = Left ("usage: myque new <title> [options]\n" <> newUsage)
  | otherwise = go (New (T.unwords titleWords) Nothing Nothing [] Nothing) opts
 where
  (titleWords, opts) = break (T.isPrefixOf "--") args

  go cmd [] = Right cmd
  go (New title kind key tags parent) (flag : more) = case (flag, more) of
    ("--kind", v : rest) -> parseKind v >>= \k -> go (New title (Just k) key tags parent) rest
    ("--key", v : rest) -> parseKey v >>= \k -> go (New title kind (Just k) tags parent) rest
    ("--tag", v : rest) -> go (New title kind key (nub (tags <> [v])) parent) rest
    ("--parent", v : rest) -> parseSelector v >>= \p -> go (New title kind key tags (Just p)) rest
    (_, []) -> Left ("missing value for " <> T.unpack flag)
    _ -> Left ("unknown option for 'new': " <> T.unpack flag <> "\n" <> newUsage)
  go cmd _ = Right cmd

-- | The option summary for @new@.
newUsage :: String
newUsage = "options: --kind <kind> --key <key> --tag <tag> --parent <item>"

-- | Parse @list@ filters.
parseFilters :: Filters -> [Text] -> Either String Filters
parseFilters acc [] = Right acc
parseFilters acc (flag : more) = case (flag, more) of
  ("--state", v : rest) -> parseState v >>= \s -> parseFilters acc {filterStates = filterStates acc <> [s]} rest
  ("--kind", v : rest) -> parseKind v >>= \k -> parseFilters acc {filterKinds = filterKinds acc <> [k]} rest
  ("--tag", v : rest) -> parseFilters acc {filterTags = filterTags acc <> [v]} rest
  ("--parent", v : rest) -> parseSelector v >>= \p -> parseFilters acc {filterParent = Just p} rest
  ("--ready", rest) -> parseFilters acc {filterReady = True} rest
  (_, []) -> Left ("missing value for " <> T.unpack flag)
  _ -> Left ("unknown option for 'list': " <> T.unpack flag)

-- | The top-level help text.
usage :: String
usage =
  unlines
    [ "myque - a local-first, Git-native work item tracker (work-item/v1)"
    , ""
    , "usage: myque <command> [arguments]"
    , ""
    , "an <item> is a canonical UUID or a human key; UUIDs win when both could match"
    , ""
    , "storage"
    , "  init                          create .tasks/ in the current directory"
    , "  check                         validate every item; exits non-zero on findings"
    , ""
    , "items"
    , "  new <title> [options]         create an item (--kind --key --tag --parent)"
    , "  show <item>                   print one item in full"
    , "  list [filters]                list items (--state --kind --tag --parent --ready)"
    , "  next                          list ready items"
    , "  query <expression>            e.g. 'state = open and kind = milestone'"
    , "  rm <item>                     delete an item's canonical file"
    , ""
    , "state"
    , "  start <item>                  state = active"
    , "  close <item>                  state = done, closed = now"
    , "  cancel <item>                 state = cancelled, closed = now"
    , "  reopen <item>                 state = open, closed cleared"
    , "  defer <item>                  state = deferred"
    , "  block <item>                  state = blocked"
    , ""
    , "metadata"
    , "  key <item> <key>              set the human-readable alias"
    , "  title <item> <title>          rewrite the body's title heading"
    , "  tag <item> <tag>...           add tags"
    , "  untag <item> <tag>...         remove tags"
    , ""
    , "relationships (accept keys, always persist UUIDs)"
    , "  depend <item> <dependency>    <item> waits until <dependency> is done"
    , "  undepend <item> <dependency>"
    , "  parent <item> [<parent>]      set or clear the parent"
    , "  relate <item> <other>         weak association, recorded on both items"
    , "  unrelate <item> <other>"
    , "  duplicate <item> <canonical>  mark <item> a duplicate and cancel it"
    , "  supersede <new> <old>         record that <new> replaces <old>"
    , ""
    , "views (generated, never authoritative)"
    , "  graph                         Mermaid flowchart of parent and dependency edges"
    , "  render                        Markdown summary grouped by state"
    , ""
    , "kinds:  " <> unwords (map (T.unpack . kindText) allKinds)
    , "states: " <> unwords (map (T.unpack . stateText) allStates)
    ]

-- | Entry point: parse arguments, execute, and set the exit status.
main :: IO ()
main = do
  args <- map T.pack <$> getArgs
  case parseCommand args of
    Left err -> die err
    Right cmd ->
      runCommand cmd >>= \case
        Left err -> die err
        Right output -> unless (T.null output) (TIO.putStr output)
 where
  die err = hPutStrLn stderr err >> exitWith (ExitFailure 1)

{- | Execute a command, returning its output or an error message. @check@
returns its findings as an error so the process exits non-zero.
-}
runCommand :: Command -> IO (Either String Text)
runCommand Help = pure (Right (T.pack usage))
runCommand Init = do
  cwd <- getCurrentDirectory
  layout <- initLayout cwd
  pure (Right ("initialised tracker in " <> T.pack (layoutRoot layout) <> "/.tasks\n"))
runCommand cmd = do
  cwd <- getCurrentDirectory
  discoverLayout cwd >>= \case
    Left err -> pure (Left err)
    Right layout -> do
      store <- loadStore layout
      unless (cmd == Check) (warnLoadErrors store)
      case cmd of
        Check -> pure (check store)
        New title kind key tags parent -> create store title kind key tags parent
        _ -> maybe (mutate store cmd) pure (readOnly store cmd)

{- | Report undecodable files on stderr. @check@ reports them as findings
instead, so it suppresses this.
-}
warnLoadErrors :: Store -> IO ()
warnLoadErrors store =
  forM_ (storeLoadErrors store) $ \err ->
    hPutStrLn stderr ("warning: " <> T.unpack (formatLoadError err))

-- | Commands that only read canonical state.
readOnly :: Store -> Command -> Maybe (Either String Text)
readOnly store = \case
  Show sel -> Just (itemDetail store (edgesOf store) <$> resolveSelector store sel)
  List filters -> Just (listItems store filters)
  Next -> Just (Right (itemRows store (readyItems store)))
  Query expression -> Just (itemRows store <$> (parseQuery expression >>= runQuery store))
  Graph -> Just (Right (mermaidGraph store))
  Render -> Just (Right (markdownSummary store))
  _ -> Nothing

-- | @list@ with its filters applied.
listItems :: Store -> Filters -> Either String Text
listItems store filters = do
  parent <- traverse (fmap itemId . resolveSelector store) (filterParent filters)
  let edges = edgesOf store
      keep item =
        and
          [ null (filterStates filters) || itemState item `elem` filterStates filters
          , null (filterKinds filters) || itemKind item `elem` filterKinds filters
          , all (`elem` itemTags item) (filterTags filters)
          , maybe True (\p -> itemParent item == Just p) parent
          , not (filterReady filters) || isReady store edges item
          ]
  pure (itemRows store (filter keep (storeItems store)))

-- | Validate the repository, reporting findings as a failure.
check :: Store -> Either String Text
check store
  | null findings = Right ("checked " <> count <> " work item(s): no findings\n")
  | otherwise = Left (T.unpack (T.unlines (map findingText findings <> [summary])))
 where
  findings = validate store
  count = T.pack (show (length (storeItems store)))
  summary = T.pack (show (length findings)) <> " finding(s)"

-- | Create a new item with a locally generated UUIDv7.
create :: Store -> Text -> Maybe Kind -> Maybe Key -> [Text] -> Maybe Selector -> IO (Either String Text)
create store title kind key tags parentSel = do
  uuid <- newUuidV7
  now <- currentTimestamp
  case build uuid now of
    Left err -> pure (Left err)
    Right item -> do
      path <- saveItem (storeLayout store) item
      pure (Right (T.unlines ["created " <> label item, uuidText (itemId item), relativeTo store path]))
 where
  build uuid now = do
    parent <- traverse (fmap itemId . resolveSelector store) parentSel
    forM_ key $ \k ->
      when (Map.member (keyText k) (storeByKey store)) $
        Left ("key already in use: " <> T.unpack (keyText k))
    let fresh = newWorkItem uuid (fromMaybe Task kind) now title
    pure fresh {itemKey = key, itemTags = tags, itemParent = parent}

-- | Plan a mutating command, then perform its writes.
mutate :: Store -> Command -> IO (Either String Text)
mutate store cmd = do
  now <- currentTimestamp
  case plan store now cmd of
    Left err -> pure (Left err)
    Right (Deleted item) -> do
      path <- deleteItem store item
      pure (Right (T.unlines ["deleted " <> label item, relativeTo store path]))
    Right (Written items) -> do
      forM_ items (saveItem (storeLayout store))
      pure (Right (T.unlines (map describe items)))
 where
  describe item =
    "updated "
      <> label item
      <> " ("
      <> stateText (itemState item)
      <> ") -> "
      <> relativeTo store (itemPath (storeLayout store) (itemId item))

-- | A path relative to the repository root, for output.
relativeTo :: Store -> FilePath -> Text
relativeTo store = T.pack . makeRelative (layoutRoot (storeLayout store))

-- | The effect of a mutating command.
data Effect
  = -- | Items to write back, already updated.
    Written [WorkItem]
  | -- | An item whose canonical file is to be removed.
    Deleted WorkItem
  deriving (Eq, Show)

{- | Compute the effect of a mutating command. Pure: it rejects
self-relationships and edges that would introduce a cycle before anything is
written.
-}
plan :: Store -> Timestamp -> Command -> Either String Effect
plan store now cmd = case cmd of
  Transit t sel -> do
    item <- resolveSelector store sel
    written . pure <$> applyTransition now t item
  SetKey sel raw -> do
    item <- resolveSelector store sel
    key <- parseKey raw
    case Map.lookup (keyText key) (storeByKey store) of
      Just owner | owner /= itemId item -> Left ("key already in use: " <> T.unpack (keyText key))
      _ -> Right (written [item {itemKey = Just key}])
  SetTitle sel title -> do
    item <- resolveSelector store sel
    Right (written [setTitle title item])
  Tag adding sel tags -> do
    item <- resolveSelector store sel
    let updated
          | adding = nub (itemTags item <> tags)
          | otherwise = filter (`notElem` tags) (itemTags item)
    Right (written [item {itemTags = updated}])
  Depend adding sel depSel -> do
    item <- resolveSelector store sel
    dep <- resolveSelector store depSel
    when (itemId item == itemId dep) (Left "an item cannot depend on itself")
    when (adding && Set.member (itemId item) (dependencyClosure store (itemId dep))) $
      Left ("that dependency would create a cycle: " <> T.unpack (label dep) <> " already depends on " <> T.unpack (label item))
    let updated
          | adding = nub (itemDepends item <> [itemId dep])
          | otherwise = filter (/= itemId dep) (itemDepends item)
    Right (written [item {itemDepends = updated}])
  Parent sel parentSel -> do
    item <- resolveSelector store sel
    traverse (resolveSelector store) parentSel >>= \case
      Nothing -> Right (written [item {itemParent = Nothing}])
      Just p -> do
        when (itemId item == itemId p) (Left "an item cannot be its own parent")
        when (itemId p `elem` descendantsOf (edgesOf store) item) $
          Left ("that parent would create a cycle: " <> T.unpack (label p) <> " is already a descendant")
        Right (written [item {itemParent = Just (itemId p)}])
  Relate adding sel otherSel -> do
    item <- resolveSelector store sel
    other <- resolveSelector store otherSel
    when (itemId item == itemId other) (Left "an item cannot be related to itself")
    let link a b
          | adding = a {itemRelated = nub (itemRelated a <> [itemId b])}
          | otherwise = a {itemRelated = filter (/= itemId b) (itemRelated a)}
    Right (written [link item other, link other item])
  Duplicate sel canonicalSel -> do
    item <- resolveSelector store sel
    canonical <- resolveSelector store canonicalSel
    when (itemId item == itemId canonical) (Left "an item cannot be a duplicate of itself")
    cancelled <- applyTransition now ToCancelled item {itemDuplicateOf = Just (itemId canonical)}
    Right (written [cancelled])
  Supersede newSel oldSel -> do
    newer <- resolveSelector store newSel
    older <- resolveSelector store oldSel
    when (itemId newer == itemId older) (Left "an item cannot supersede itself")
    Right (written [newer {itemSupersedes = nub (itemSupersedes newer <> [itemId older])}])
  Remove sel -> Deleted <$> resolveSelector store sel
  _ -> Left "internal error: not a mutating command"
 where
  written = Written . map (\item -> item {itemUpdated = Just now})

{- | Apply a state transition, keeping @closed@ consistent: terminal states
gain a timestamp, non-terminal states lose theirs.
-}
applyTransition :: Timestamp -> Transition -> WorkItem -> Either String WorkItem
applyTransition now t item
  | target == itemState item && isJust (itemClosed item) == terminal =
      Left (T.unpack (label item) <> " is already " <> T.unpack (stateText target))
  | otherwise = Right item {itemState = target, itemClosed = if terminal then Just closed else Nothing}
 where
  target = transitionState t
  terminal = target `elem` [Done, Cancelled]
  closed = fromMaybe now (itemClosed item)

{- | Every item reachable by following dependency edges from an ID, the ID
included. Used to reject an edge that would close a dependency cycle.
-}
dependencyClosure :: Store -> Uuid -> Set.Set Uuid
dependencyClosure store = go Set.empty
 where
  edges = edgesOf store
  go seen uuid
    | Set.member uuid seen = seen
    | otherwise = case Map.lookup uuid (storeById store) of
        Nothing -> Set.insert uuid seen
        Just item -> foldl go (Set.insert uuid seen) (dependenciesOf edges item)
