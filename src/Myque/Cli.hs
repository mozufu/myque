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

Every value a command writes is validated before the write, so the store
never refuses to load an item the CLI itself created.

Exit status distinguishes the failures a CI gate has to tell apart:

* @0@ — success.
* @1@ — @check@ reported findings. Findings are the command's result rather
  than a diagnostic, so they are written to stdout.
* @2@ — a usage error, a missing tracker, or unreadable configuration.
* @3@ — a well-formed command that could not be carried out.
-}
module Myque.Cli
  ( Command (..)
  , Filters (..)
  , emptyFilters
  , Format (..)
  , parseFormat
  , Transition (..)
  , transitionState
  , parseCommand
  , runCommand
  , Failure (..)
  , failureStatus
  , plan
  , Effect (..)
  , usage
  , commandUsage
  , main
  ) where

import Control.Monad (forM_, unless, when)
import Data.Bifunctor (first)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Version (showVersion)
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
  , parseTag
  , setTitle
  , stateText
  )
import Myque.Query (parseQuery, runQuery)
import Myque.Render (idLines, itemDetail, itemRows, jsonLines, label, markdownSummary, mermaidGraph)
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
import Paths_myque (version)
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
  | -- | @myque list [--state --kind --tag --parent --ready --format]@
    List Filters Format
  | -- | @myque next [--format]@
    Next Format
  | -- | @myque check@
    Check
  | -- | @myque start|close|cancel|reopen|defer|block \<item\>@
    Transit Transition Selector
  | -- | @myque key \<item\> \<key\>@; 'Nothing' is @--unset@
    SetKey Selector (Maybe Text)
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
  | -- | @myque query \<expression\> [--format]@
    Query Text Format
  | -- | @myque graph@
    Graph
  | -- | @myque render@
    Render
  | -- | @myque --version@
    Version
  | -- | @myque help [\<command\>]@
    Help (Maybe Text)
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

{- | The output form of a listing command. The machine-readable forms exist
so that a script can enumerate canonical identities without parsing a table
whose 'label' column is deliberately abbreviated.
-}
data Format
  = -- | The human-readable @KEY KIND STATE TITLE@ table.
    Table
  | -- | One canonical 36-character ID per line.
    Ids
  | -- | NDJSON: one object per item, every relationship a canonical ID.
    Json
  deriving (Eq, Show)

-- | Parse a @--format@ value. An unrecognised name is an error, never a fallback.
parseFormat :: Text -> Either String Format
parseFormat = \case
  "table" -> Right Table
  "id" -> Right Ids
  "json" -> Right Json
  other -> Left ("invalid format: " <> T.unpack other <> " (expected one of table, id, json)")

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
parseCommand [] = Right (Help Nothing)
parseCommand (verb : rest)
  | verb `elem` ["version", "--version", "-V"] =
      if null rest then Right Version else Left "myque version takes no arguments"
  | verb `elem` ["help", "--help", "-h"] = case rest of
      [] -> Right (Help Nothing)
      [topic] -> helpFor topic
      _ -> Left "usage: myque help [<command>]"
  | any (`elem` ["--help", "-h"]) rest = helpFor verb
  | otherwise = case verb of
      "init" -> noArgs Init
      "new" -> parseNew rest
      "show" -> Show <$> oneSelector
      "list" -> uncurry List <$> parseListArgs rest
      "next" -> Next <$> parseFormatOnly "next" rest
      "check" -> noArgs Check
      "start" -> transit ToActive
      "close" -> transit ToDone
      "cancel" -> transit ToCancelled
      "reopen" -> transit ToOpen
      "defer" -> transit ToDeferred
      "block" -> transit ToBlocked
      "key" -> parseSetKey rest
      "title" -> case rest of
        target : titleWords
          | not (null titleWords) -> SetTitle <$> parseSelector target <*> pure (T.unwords titleWords)
        _ -> Left (usageOf "title")
      "tag" -> tags True
      "untag" -> tags False
      "depend" -> two (relation (Depend True))
      "undepend" -> two (relation (Depend False))
      "parent" -> case rest of
        [target] -> Parent <$> parseSelector target <*> pure Nothing
        [target, parent] -> Parent <$> parseSelector target <*> (Just <$> parseSelector parent)
        _ -> Left (usageOf "parent")
      "relate" -> two (relation (Relate True))
      "unrelate" -> two (relation (Relate False))
      "duplicate" -> two (relation Duplicate)
      "supersede" -> two (relation Supersede)
      "rm" -> Remove <$> oneSelector
      "query" -> parseQueryArgs rest
      "graph" -> noArgs Graph
      "render" -> noArgs Render
      _ -> Left ("unknown command: " <> T.unpack verb <> "\n\n" <> usage)
 where
  helpFor topic
    | isJust (lookup topic commandDocs) = Right (Help (Just topic))
    | topic `elem` ["help", "version"] = Right (Help (Just topic))
    | otherwise = Left ("unknown command: " <> T.unpack topic <> "\n\n" <> usage)

  transit t = Transit t <$> oneSelector

  noArgs cmd
    | null rest = Right cmd
    | otherwise = Left ("myque " <> T.unpack verb <> " takes no arguments")

  oneSelector = case rest of
    [target] -> parseSelector target
    _ -> Left (usageOf verb)

  two build = case rest of
    [a, b] -> build a b
    _ -> Left (usageOf verb)

  relation build a b = build <$> parseSelector a <*> parseSelector b

  -- 'tag' validates, 'untag' does not: removal never writes a tag, and
  -- refusing a malformed one would make it unremovable.
  tags adding = case rest of
    target : ts
      | not (null ts) ->
          Tag adding
            <$> parseSelector target
            <*> (nub <$> if adding then traverse parseTag ts else pure ts)
    _ -> Left (usageOf verb)

-- | Parse @key@: set an alias, or drop it with @--unset@.
parseSetKey :: [Text] -> Either String Command
parseSetKey = \case
  [target, "--unset"] -> SetKey <$> parseSelector target <*> pure Nothing
  [target, raw]
    | T.isPrefixOf "--" raw -> Left (optionError "key" raw [])
    | otherwise -> SetKey <$> parseSelector target <*> pure (Just raw)
  _ -> Left (usageOf "key")

-- | Parse @new@: title words up to the first option, then the options.
parseNew :: [Text] -> Either String Command
parseNew args
  | null titleWords = Left (usageOf "new")
  | otherwise = go (New (T.unwords titleWords) Nothing Nothing [] Nothing) opts
 where
  (titleWords, opts) = break (T.isPrefixOf "--") args

  go cmd [] = Right cmd
  go (New title kind key tags parent) (flag : more) = case (flag, more) of
    ("--kind", v : rest) -> parseKind v >>= \k -> go (New title (Just k) key tags parent) rest
    ("--key", v : rest) -> parseKey v >>= \k -> go (New title kind (Just k) tags parent) rest
    ("--tag", v : rest) -> parseTag v >>= \t -> go (New title kind key (nub (tags <> [t])) parent) rest
    ("--parent", v : rest) -> parseSelector v >>= \p -> go (New title kind key tags (Just p)) rest
    _ -> Left (optionError "new" flag more)
  go cmd _ = Right cmd

-- | Parse @list@ filters and output format.
parseListArgs :: [Text] -> Either String (Filters, Format)
parseListArgs = go (emptyFilters, Table)
 where
  go acc [] = Right acc
  go (f, fmt) (flag : more) = case (flag, more) of
    ("--state", v : rest) -> parseState v >>= \s -> go (f {filterStates = filterStates f <> [s]}, fmt) rest
    ("--kind", v : rest) -> parseKind v >>= \k -> go (f {filterKinds = filterKinds f <> [k]}, fmt) rest
    ("--tag", v : rest) -> parseTag v >>= \t -> go (f {filterTags = filterTags f <> [t]}, fmt) rest
    ("--parent", v : rest) -> parseSelector v >>= \p -> go (f {filterParent = Just p}, fmt) rest
    ("--format", v : rest) -> parseFormat v >>= \x -> go (f, x) rest
    ("--ready", rest) -> go (f {filterReady = True}, fmt) rest
    _ -> Left (optionError "list" flag more)

-- | Parse a command whose only option is @--format@.
parseFormatOnly :: Text -> [Text] -> Either String Format
parseFormatOnly verb = go Table
 where
  go acc [] = Right acc
  go _ (flag : more) = case (flag, more) of
    ("--format", v : rest) -> parseFormat v >>= \f -> go f rest
    _ -> Left (optionError verb flag more)

{- | Parse @query@: the expression words, with @--format@ accepted on either
side of them.
-}
parseQueryArgs :: [Text] -> Either String Command
parseQueryArgs args = do
  (words', fmt) <- go ([], Table) args
  if null words'
    then Left (usageOf "query")
    else Right (Query (T.unwords words') fmt)
 where
  go acc [] = Right acc
  go (ws, fmt) (word : more) = case (word, more) of
    ("--format", v : rest) -> parseFormat v >>= \f -> go (ws, f) rest
    _
      | T.isPrefixOf "--" word -> Left (optionError "query" word more)
      | otherwise -> go (ws <> [word], fmt) more

{- | The error for an argument a command cannot use: an unknown option, a
known option missing its value, or a stray positional word. An unknown
option names every option the command does accept, so the reader does not
have to go looking for the help text.
-}
optionError :: Text -> Text -> [Text] -> String
optionError verb flag more
  | not (T.isPrefixOf "-" flag) =
      "unexpected argument for '" <> T.unpack verb <> "': " <> T.unpack flag <> "\n" <> usageOf verb
  | flag `elem` optionsOf verb && null more =
      "missing value for " <> T.unpack flag <> "\n" <> usageOf verb
  | otherwise =
      "unknown option for '"
        <> T.unpack verb
        <> "': "
        <> T.unpack flag
        <> "\n"
        <> usageOf verb
        <> "\n"
        <> optionSummary verb

-- | The option names a command accepts, taken from its documentation.
optionsOf :: Text -> [Text]
optionsOf verb =
  [ T.takeWhile (/= ' ') (T.pack option)
  | (option, _) <- maybe [] docOptions (lookup verb commandDocs)
  ]

-- | The options a command accepts, on one line, for an error message.
optionSummary :: Text -> String
optionSummary verb = case maybe [] docOptions (lookup verb commandDocs) of
  [] -> "'" <> T.unpack verb <> "' takes no options"
  options -> "options: " <> unwords (map fst options)

-- | One command's documentation. The single source of both help texts.
data CommandDoc = CommandDoc
  { docSynopsis :: String
  -- ^ The argument form, without the @myque@ prefix.
  , docSummary :: String
  -- ^ One line, as shown in the command list.
  , docOptions :: [(String, String)]
  -- ^ Each option's form and what it does.
  }

-- | Every command, in help order, grouped as the top-level help prints them.
commandGroups :: [(String, [(Text, CommandDoc)])]
commandGroups =
  [
    ( "storage"
    ,
      [ ("init", CommandDoc "init" "create .tasks/ in the current directory" [])
      , ("check", CommandDoc "check" "validate every item; exit 1 on findings" [])
      ]
    )
  ,
    ( "items"
    ,
      [
        ( "new"
        , CommandDoc
            "new <title> [options]"
            "create an item with a locally allocated UUIDv7"
            [ ("--kind <kind>", "one of " <> unwords (map (T.unpack . kindText) allKinds))
            , ("--key <key>", "human alias, e.g. C9.4")
            , ("--tag <tag>", "add a tag; repeatable")
            , ("--parent <item>", "set the parent")
            ]
        )
      , ("show", CommandDoc "show <item>" "print one item in full" [])
      ,
        ( "list"
        , CommandDoc
            "list [filters]"
            "list items, most recently created last"
            [ ("--state <state>", "keep items in that state; repeatable")
            , ("--kind <kind>", "keep items of that kind; repeatable")
            , ("--tag <tag>", "keep items carrying that tag; repeatable")
            , ("--parent <item>", "keep children of that item")
            , ("--ready", "keep only ready items")
            , ("--format <format>", formatSummary)
            ]
        )
      , ("next", CommandDoc "next [--format <format>]" "list ready items" [("--format <format>", formatSummary)])
      ,
        ( "query"
        , CommandDoc
            "query <expression> [--format <format>]"
            "e.g. 'state = open and kind = milestone'"
            [("--format <format>", formatSummary)]
        )
      , ("rm", CommandDoc "rm <item>" "delete an item's canonical file" [])
      ]
    )
  ,
    ( "state"
    ,
      [ ("start", CommandDoc "start <item>" "state = active" [])
      , ("close", CommandDoc "close <item>" "state = done, closed = now" [])
      , ("cancel", CommandDoc "cancel <item>" "state = cancelled, closed = now" [])
      , ("reopen", CommandDoc "reopen <item>" "state = open, closed cleared" [])
      , ("defer", CommandDoc "defer <item>" "state = deferred" [])
      , ("block", CommandDoc "block <item>" "state = blocked" [])
      ]
    )
  ,
    ( "metadata"
    ,
      [
        ( "key"
        , CommandDoc
            "key <item> <key>|--unset"
            "set or drop the human-readable alias"
            [("--unset", "remove the key, leaving relationships untouched")]
        )
      , ("title", CommandDoc "title <item> <title>" "rewrite the body's title heading" [])
      , ("tag", CommandDoc "tag <item> <tag>..." "add tags" [])
      , ("untag", CommandDoc "untag <item> <tag>..." "remove tags" [])
      ]
    )
  ,
    ( "relationships (accept keys, always persist UUIDs)"
    ,
      [ ("depend", CommandDoc "depend <item> <dependency>" "<item> waits until <dependency> is done" [])
      , ("undepend", CommandDoc "undepend <item> <dependency>" "remove a dependency edge" [])
      , ("parent", CommandDoc "parent <item> [<parent>]" "set or clear the parent" [])
      , ("relate", CommandDoc "relate <item> <other>" "weak association, recorded on both items" [])
      , ("unrelate", CommandDoc "unrelate <item> <other>" "remove a weak association" [])
      , ("duplicate", CommandDoc "duplicate <item> <canonical>" "mark <item> a duplicate and cancel it" [])
      , ("supersede", CommandDoc "supersede <new> <old>" "record that <new> replaces <old>" [])
      ]
    )
  ,
    ( "views (generated, never authoritative)"
    ,
      [ ("graph", CommandDoc "graph" "Mermaid flowchart of parent and dependency edges" [])
      , ("render", CommandDoc "render" "Markdown summary grouped by state" [])
      ]
    )
  ]

-- | The documentation of every command, by verb.
commandDocs :: [(Text, CommandDoc)]
commandDocs = concatMap snd commandGroups

-- | What the @--format@ values mean, shared by every command that has one.
formatSummary :: String
formatSummary = "table (default), id (one canonical id per line), or json (NDJSON)"

-- | The top-level help text.
usage :: String
usage =
  unlines $
    [ "myque - a local-first, Git-native work item tracker (work-item/v1)"
    , ""
    , "usage: myque <command> [arguments]"
    , "       myque <command> --help"
    , "       myque --version"
    , ""
    , "an <item> is a canonical UUID, an unambiguous prefix of one, or a human"
    , "key; ids win when both could match"
    ]
      <> concatMap group commandGroups
      <> [ ""
         , "exit status"
         , "  0  success"
         , "  1  check reported findings, written to stdout"
         , "  2  usage error, missing tracker, or unreadable configuration"
         , "  3  the command could not be carried out"
         , ""
         , "kinds:  " <> unwords (map (T.unpack . kindText) allKinds)
         , "states: " <> unwords (map (T.unpack . stateText) allStates)
         ]
 where
  group (name, commands) = "" : name : map entry commands
  entry (_, doc) = "  " <> pad 30 (docSynopsis doc) <> docSummary doc

{- | One command's help: its synopsis, what it does, and every option it
accepts. @help@ and @version@ have no entry of their own, so they fall back
to the top-level text.
-}
commandUsage :: Text -> String
commandUsage verb = case lookup verb commandDocs of
  Nothing -> usage
  Just doc ->
    unlines $
      [ "usage: myque " <> docSynopsis doc
      , ""
      , docSummary doc
      ]
        <> options doc
 where
  options doc
    | null (docOptions doc) = []
    | otherwise = "" : "options:" : ["  " <> pad 20 option <> what | (option, what) <- docOptions doc]

-- | Left-align a help column.
pad :: Int -> String -> String
pad width text = text <> replicate (max 1 (width - length text)) ' '

-- | The usage line of a command, for an argument error.
usageOf :: Text -> String
usageOf verb = case lookup verb commandDocs of
  Just doc -> "usage: myque " <> docSynopsis doc
  Nothing -> "usage: myque <command> [arguments]"

-- | Why the process exited non-zero. The constructor fixes the exit status.
data Failure
  = -- | @check@ reported findings: the command's result, so stdout carries it.
    Findings Text
  | -- | A usage error, a missing tracker, or unreadable configuration.
    Usage String
  | -- | A well-formed command that could not be carried out.
    Rejected String
  deriving (Eq, Show)

{- | The process exit status of a failure. Documented in @myque help@ and in
the specification's validation section, so a CI gate can distinguish an
invalid tracker from a missing one.
-}
failureStatus :: Failure -> Int
failureStatus = \case
  Findings _ -> 1
  Usage _ -> 2
  Rejected _ -> 3

-- | Entry point: parse arguments, execute, and set the exit status.
main :: IO ()
main = do
  args <- map T.pack <$> getArgs
  case parseCommand args of
    Left err -> quit (Usage err)
    Right cmd ->
      runCommand cmd >>= \case
        Left failure -> quit failure
        Right output -> unless (T.null output) (TIO.putStr output)
 where
  quit failure = do
    case failure of
      Findings findings -> TIO.putStr findings
      Usage err -> hPutStrLn stderr err
      Rejected err -> hPutStrLn stderr err
    exitWith (ExitFailure (failureStatus failure))

{- | Execute a command, returning its output or the failure that stopped it.
@check@ returns its findings as a 'Findings' failure so the process exits
non-zero while still printing them as the command's result.
-}
runCommand :: Command -> IO (Either Failure Text)
runCommand (Help topic) = pure (Right (T.pack (maybe usage commandUsage topic)))
runCommand Version = pure (Right (T.pack (showVersion version) <> "\n"))
runCommand Init = do
  cwd <- getCurrentDirectory
  layout <- initLayout cwd
  pure (Right ("initialised tracker in " <> T.pack (layoutRoot layout) <> "/.tasks\n"))
runCommand cmd = do
  cwd <- getCurrentDirectory
  discoverLayout cwd >>= \case
    Left err -> pure (Left (Usage err))
    Right layout -> do
      store <- loadStore layout
      unless (cmd == Check) (warnLoadErrors store)
      case cmd of
        Check -> pure (check store)
        New title kind key tags parent -> first Rejected <$> create store title kind key tags parent
        _ -> case readOnly store cmd of
          Just result -> pure (first Rejected result)
          Nothing -> first Rejected <$> mutate store cmd

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
  List filters fmt -> Just (render fmt <$> selection store filters)
  Next fmt -> Just (Right (render fmt (readyItems store)))
  Query expression fmt -> Just (render fmt <$> (parseQuery expression >>= runQuery store))
  Graph -> Just (Right (mermaidGraph store))
  Render -> Just (Right (markdownSummary store))
  _ -> Nothing
 where
  render = \case
    Table -> itemRows store
    Ids -> idLines
    Json -> jsonLines store

-- | The items @list@ selects: every filter must hold.
selection :: Store -> Filters -> Either String [WorkItem]
selection store filters = do
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
  pure (filter keep (storeItems store))

-- | Validate the repository, reporting findings as a 'Findings' failure.
check :: Store -> Either Failure Text
check store
  | null findings = Right ("checked " <> count <> " work item(s): no findings\n")
  | otherwise = Left (Findings (T.unlines (map findingText findings <> [summary])))
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
  SetKey sel Nothing -> do
    item <- resolveSelector store sel
    Right (written [item {itemKey = Nothing}])
  SetKey sel (Just raw) -> do
    item <- resolveSelector store sel
    key <- parseKey raw
    case Map.lookup (keyText key) (storeByKey store) of
      Just owner | owner /= itemId item -> Left ("key already in use: " <> T.unpack (keyText key))
      _ -> Right (written [item {itemKey = Just key}])
  SetTitle sel title -> do
    item <- resolveSelector store sel
    Right (written [setTitle title item])
  Tag adding sel raw -> do
    item <- resolveSelector store sel
    -- Validated on the way in only: a tag being removed is never written.
    tags <- if adding then traverse parseTag raw else pure raw
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
