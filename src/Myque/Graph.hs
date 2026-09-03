{- | The relationship graph.

@depends@ is the canonical dependency edge and @blocks@ is its inverse, so
the two are normalised into one dependency relation: an item is
dependency-ready when every item it depends on — declared from either side —
is @done@. A reference that does not resolve is never satisfied, so a
dangling edge cannot make an item ready.

The parent and dependency relations must both be acyclic. 'parentCycles' and
'dependencyCycles' return the members of each offending cycle so that
validation can name them.
-}
module Myque.Graph
  ( Edges
  , edgesOf
  , dependenciesOf
  , blockedBy
  , childrenOf
  , isReady
  , readyItems
  , parentCycles
  , dependencyCycles
  , ancestorsOf
  , descendantsOf
  ) where

import Data.List (nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Myque.Item (State (..), WorkItem (..))
import Myque.Store (Store (..), storeItems)
import Myque.Uuid (Uuid)

{- | The normalised edge index of a store: dependency edges per item and the
child edges of the parent relation. Built once and shared, so graph queries
do not rescan every item.
-}
data Edges = Edges
  { edgeDepends :: Map Uuid [Uuid]
  {- ^ @depends@ unioned with the inverse of others' @blocks@, including
  references that do not resolve.
  -}
  , edgeChildren :: Map Uuid [Uuid]
  }

-- | Build the edge index for a store.
edgesOf :: Store -> Edges
edgesOf store =
  Edges
    { edgeDepends = Map.fromList [(itemId i, sort (nub (itemDepends i <> inverse (itemId i)))) | i <- items]
    , edgeChildren = Map.fromListWith (<>) [(p, [itemId i]) | i <- items, Just p <- [itemParent i]]
    }
 where
  items = storeItems store
  blocksIndex = Map.fromListWith (<>) [(target, [itemId i]) | i <- items, target <- itemBlocks i]
  inverse uuid = Map.findWithDefault [] uuid blocksIndex

-- | The effective dependencies of an item, dangling references included.
dependenciesOf :: Edges -> WorkItem -> [Uuid]
dependenciesOf edges item = Map.findWithDefault [] (itemId item) (edgeDepends edges)

-- | The items that depend on an item: the inverse of 'dependenciesOf'.
blockedBy :: Edges -> WorkItem -> [Uuid]
blockedBy edges item =
  sort [dependent | (dependent, deps) <- Map.toList (edgeDepends edges), itemId item `elem` deps]

-- | The items whose @parent@ is this item.
childrenOf :: Edges -> WorkItem -> [Uuid]
childrenOf edges item = sort (Map.findWithDefault [] (itemId item) (edgeChildren edges))

-- | Whether an item is @open@ with every dependency @done@.
isReady :: Store -> Edges -> WorkItem -> Bool
isReady store edges item = itemState item == Open && all satisfied (dependenciesOf edges item)
 where
  satisfied uuid = fmap itemState (Map.lookup uuid (storeById store)) == Just Done

-- | Every ready item, in store order.
readyItems :: Store -> [WorkItem]
readyItems store = filter (isReady store edges) (storeItems store)
 where
  edges = edgesOf store

-- | The members of each @parent@ cycle, self-parents included.
parentCycles :: Store -> [[Uuid]]
parentCycles store = cyclesIn (Map.map (maybe [] pure . itemParent) (storeById store))

-- | The members of each dependency cycle, @blocks@ edges included.
dependencyCycles :: Store -> [[Uuid]]
dependencyCycles store = cyclesIn (edgeDepends (edgesOf store))

-- | The transitive @parent@ chain of an item, nearest first, cycle-safe.
ancestorsOf :: Store -> WorkItem -> [Uuid]
ancestorsOf store = walk Set.empty
 where
  walk seen item = case itemParent item of
    Just p
      | not (Set.member p seen)
      , Just parent <- Map.lookup p (storeById store) ->
          p : walk (Set.insert p seen) parent
    _ -> []

-- | The transitive children of an item, breadth-first, cycle-safe.
descendantsOf :: Edges -> WorkItem -> [Uuid]
descendantsOf edges item = go Set.empty (childrenOf edges item)
 where
  go _ [] = []
  go seen (uuid : queue)
    | Set.member uuid seen = go seen queue
    | otherwise = uuid : go (Set.insert uuid seen) (queue <> Map.findWithDefault [] uuid (edgeChildren edges))

{- | The cyclic strongly connected components of an adjacency map: every
component of more than one vertex, plus self-loops. Edges to vertices outside
the map are ignored, since a dangling reference is reported separately.
-}
cyclesIn :: Map Uuid [Uuid] -> [[Uuid]]
cyclesIn adjacency = sort (map sort (filter isCycle (components adjacency)))
 where
  isCycle [single] = single `elem` Map.findWithDefault [] single adjacency
  isCycle component = length component > 1

{- | Tarjan's strongly connected components, visiting vertices in the map's
ascending key order so the output is deterministic.
-}
components :: Map Uuid [Uuid] -> [[Uuid]]
components adjacency = reverse (found (foldl visit initial (Map.keys adjacency)))
 where
  initial = Tarjan Map.empty Map.empty [] Set.empty 0 []

  visit st n
    | Map.member n (index st) = st
    | otherwise = close n (foldl (step n) (open n st) (successors n))

  open n st =
    st
      { index = Map.insert n (counter st) (index st)
      , lowlink = Map.insert n (counter st) (lowlink st)
      , counter = counter st + 1
      , stack = n : stack st
      , onStack = Set.insert n (onStack st)
      }

  step n st m
    | not (Map.member m (index st)) =
        let st' = visit st m
         in st' {lowlink = Map.insert n (min (low st' n) (low st' m)) (lowlink st')}
    | Set.member m (onStack st) = st {lowlink = Map.insert n (min (low st n) (idx st m)) (lowlink st)}
    | otherwise = st

  close n st
    | low st n == idx st n =
        let (above, rest) = span (/= n) (stack st)
            component = above <> take 1 rest
         in st
              { stack = drop 1 rest
              , onStack = foldr Set.delete (onStack st) component
              , found = component : found st
              }
    | otherwise = st

  successors n = filter (`Map.member` adjacency) (Map.findWithDefault [] n adjacency)
  low st n = fromMaybe maxBound (Map.lookup n (lowlink st))
  idx st n = fromMaybe maxBound (Map.lookup n (index st))

-- | Tarjan's algorithm state.
data Tarjan = Tarjan
  { index :: Map Uuid Int
  , lowlink :: Map Uuid Int
  , stack :: [Uuid]
  , onStack :: Set Uuid
  , counter :: Int
  , found :: [[Uuid]]
  }
