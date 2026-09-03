{-# LANGUAGE OverloadedStrings #-}

{- | A small filter expression language.

The specification leaves the query language outside the normative scope of
schema @v1@, so this is a deliberately small predicate language over the
properties it lists: @id@, @key@, @kind@, @state@, @tag@, @parent@, @ready@,
@created@ and @closed@.

> state = open and kind = milestone
> tag = "runtime" and state != done
> ready = true and (kind = task or kind = bug)

Grammar:

> expr   ::= term ('or' term)*
> term   ::= factor ('and' factor)*
> factor ::= 'not' factor | '(' expr ')' | comparison
> comp   ::= property op value
> op     ::= '=' | '!=' | '<' | '<=' | '>' | '>='

Comparison is textual for @id@, @key@, @kind@, @state@, @parent@ and @tag@
(where @=@ means \"has this tag\"), chronological for @created@ and @closed@,
and boolean for @ready@. An unknown property or operator is a parse error, so
a typo never silently matches nothing.
-}
module Myque.Query
  ( Query
  , parseQuery
  , matches
  , runQuery
  ) where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Myque.Graph (Edges, edgesOf, isReady)
import Myque.Item (WorkItem (..), keyText, kindText, stateText)
import Myque.Store (Store, storeItems)
import Myque.Timestamp (parseTimestamp)
import Myque.Uuid (uuidText)

-- | A parsed filter expression.
data Query
  = And Query Query
  | Or Query Query
  | Not Query
  | Compare Property Operator Text
  deriving (Eq, Show)

-- | A queryable work-item property.
data Property
  = PropId
  | PropKey
  | PropKind
  | PropState
  | PropTag
  | PropParent
  | PropReady
  | PropCreated
  | PropClosed
  deriving (Eq, Show)

-- | A comparison operator.
data Operator = OpEq | OpNe | OpLt | OpLe | OpGt | OpGe
  deriving (Eq, Show)

-- | The property names the language accepts.
properties :: [(Text, Property)]
properties =
  [ ("id", PropId)
  , ("key", PropKey)
  , ("kind", PropKind)
  , ("state", PropState)
  , ("tag", PropTag)
  , ("parent", PropParent)
  , ("ready", PropReady)
  , ("created", PropCreated)
  , ("closed", PropClosed)
  ]

-- | Operator spellings, longest first so @<=@ wins over @<@.
operators :: [(Text, Operator)]
operators = [("!=", OpNe), ("<=", OpLe), (">=", OpGe), ("=", OpEq), ("<", OpLt), (">", OpGt)]

-- | A lexical token.
data Token
  = TWord Text
  | TString Text
  | TOp Operator
  | TOpen
  | TClose
  deriving (Eq, Show)

-- | Parse a query expression.
parseQuery :: Text -> Either String Query
parseQuery raw = do
  tokens <- tokenize raw
  (query, rest) <- expr tokens
  case rest of
    [] -> Right query
    TClose : _ -> Left "unbalanced ')' in query"
    t : _ -> Left ("unexpected token in query: " <> describe t)

-- | Render a token for an error message.
describe :: Token -> String
describe (TWord w) = T.unpack w
describe (TString s) = show (T.unpack s)
describe (TOp o) = T.unpack (opText o)
describe TOpen = "("
describe TClose = ")"

-- | The spelling of an operator.
opText :: Operator -> Text
opText o = maybe "=" fst (lookup o [(v, (k, v)) | (k, v) <- operators])

-- | Split a query into tokens.
tokenize :: Text -> Either String [Token]
tokenize = go
 where
  go input = case T.uncons (T.stripStart input) of
    Nothing -> Right []
    Just ('(', rest) -> (TOpen :) <$> go rest
    Just (')', rest) -> (TClose :) <$> go rest
    Just ('"', rest) -> quoted '"' rest
    Just ('\'', rest) -> quoted '\'' rest
    Just _ ->
      let stripped = T.stripStart input
       in case [(op, T.drop (T.length spelling) stripped) | (spelling, op) <- operators, spelling `T.isPrefixOf` stripped] of
            (op, rest) : _ -> (TOp op :) <$> go rest
            [] ->
              let (word, rest) = T.break boundary stripped
               in if T.null word
                    then Left ("unexpected character in query: " <> T.unpack (T.take 1 stripped))
                    else (TWord word :) <$> go rest

  quoted q rest = case T.breakOn (T.singleton q) rest of
    (_, "") -> Left "unterminated string literal in query"
    (value, after) -> (TString value :) <$> go (T.drop 1 after)

  boundary c = isSpace c || c `elem` ("()=!<>\"'" :: String)

-- | @expr ::= term ('or' term)*@
expr :: [Token] -> Either String (Query, [Token])
expr tokens = do
  (lhs, rest) <- term tokens
  case rest of
    TWord w : more | keyword "or" w -> do
      (rhs, rest') <- expr more
      pure (Or lhs rhs, rest')
    _ -> Right (lhs, rest)

-- | @term ::= factor ('and' factor)*@
term :: [Token] -> Either String (Query, [Token])
term tokens = do
  (lhs, rest) <- factor tokens
  case rest of
    TWord w : more | keyword "and" w -> do
      (rhs, rest') <- term more
      pure (And lhs rhs, rest')
    _ -> Right (lhs, rest)

-- | @factor ::= 'not' factor | '(' expr ')' | comparison@
factor :: [Token] -> Either String (Query, [Token])
factor (TWord w : rest) | keyword "not" w = do
  (inner, rest') <- factor rest
  pure (Not inner, rest')
factor (TOpen : rest) = do
  (inner, rest') <- expr rest
  case rest' of
    TClose : more -> Right (inner, more)
    _ -> Left "missing ')' in query"
factor (TWord name : TOp op : value : rest) = do
  prop <- property name
  literal <- scalar value
  pure (Compare prop op literal, rest)
factor [TWord name, TOp _] = Left ("missing value after operator for '" <> T.unpack name <> "'")
factor (TWord name : _) = Left ("missing operator after '" <> T.unpack name <> "'")
factor [] = Left "unexpected end of query"
factor (t : _) = Left ("unexpected token in query: " <> describe t)

-- | Case-insensitive keyword match.
keyword :: Text -> Text -> Bool
keyword k w = T.toLower w == k

-- | Resolve a property name.
property :: Text -> Either String Property
property name = case lookup (T.toLower name) properties of
  Just p -> Right p
  Nothing ->
    Left
      ( "unknown query property: "
          <> T.unpack name
          <> " (expected one of "
          <> T.unpack (T.intercalate ", " (map fst properties))
          <> ")"
      )

-- | A comparison right-hand side.
scalar :: Token -> Either String Text
scalar (TWord w) = Right w
scalar (TString s) = Right s
scalar t = Left ("expected a value in query, got " <> describe t)

-- | Whether an item satisfies a query.
matches :: Store -> Edges -> Query -> WorkItem -> Either String Bool
matches store edges query item = case query of
  And a b -> (&&) <$> matches store edges a item <*> matches store edges b item
  Or a b -> (||) <$> matches store edges a item <*> matches store edges b item
  Not a -> not <$> matches store edges a item
  Compare prop op value -> compareProperty store edges item prop op value

-- | Evaluate a single comparison.
compareProperty :: Store -> Edges -> WorkItem -> Property -> Operator -> Text -> Either String Bool
compareProperty store edges item prop op value = case prop of
  PropId -> textual (Just (uuidText (itemId item)))
  PropKey -> textual (keyText <$> itemKey item)
  PropKind -> textual (Just (kindText (itemKind item)))
  PropState -> textual (Just (stateText (itemState item)))
  PropParent -> textual (uuidText <$> itemParent item)
  PropTag -> case op of
    OpEq -> Right (value `elem` itemTags item)
    OpNe -> Right (value `notElem` itemTags item)
    _ -> Left "operator not supported for 'tag' (use = or !=)"
  PropReady -> do
    wanted <- boolean value
    equality (isReady store edges item == wanted)
  PropCreated -> chronological (Just (itemCreated item))
  PropClosed -> chronological (itemClosed item)
 where
  textual actual = case op of
    OpEq -> Right (actual == Just value)
    OpNe -> Right (actual /= Just value)
    _ -> Left ("operator not supported for this property (use = or !=): " <> T.unpack (opText op))

  chronological actual = do
    wanted <- either (const (Left ("not a timestamp: " <> T.unpack value))) Right (parseTimestamp value)
    pure $ case actual of
      Nothing -> op == OpNe
      Just ts -> apply op (compare ts wanted)

  equality result = case op of
    OpEq -> Right result
    OpNe -> Right (not result)
    _ -> Left "operator not supported for 'ready' (use = or !=)"

  boolean v
    | T.toLower v `elem` ["true", "yes", "1"] = Right True
    | T.toLower v `elem` ["false", "no", "0"] = Right False
    | otherwise = Left ("not a boolean: " <> T.unpack v)

-- | Apply an ordering operator to a comparison result.
apply :: Operator -> Ordering -> Bool
apply OpEq o = o == EQ
apply OpNe o = o /= EQ
apply OpLt o = o == LT
apply OpLe o = o /= GT
apply OpGt o = o == GT
apply OpGe o = o /= LT

-- | Run a query over the whole store, in store order.
runQuery :: Store -> Query -> Either String [WorkItem]
runQuery store query = filterM (matches store (edgesOf store) query) (storeItems store)
 where
  filterM p = foldr keep (Right [])
   where
    keep x acc = do
      keepIt <- p x
      rest <- acc
      pure (if keepIt then x : rest else rest)
