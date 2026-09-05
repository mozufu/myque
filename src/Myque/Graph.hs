{- | The relationship graph.

@depends@ is the canonical dependency edge and @blocks@ is its inverse, so
the two are normalised into one dependency relation: an item is
dependency-ready when every item it depends on — declared from either side —
is @done@. A reference that does not resolve is never satisfied, so a
dangling edge cannot make an item ready.

The parent and dependency relations must both be acyclic. 'parentCycles' and
'dependencyCycles' report each offending cycle as a t'Cycle': a bounded
witness path plus the edge that closes it, so a finding stays readable no
matter how long the cycle is.
-}
module Myque.Graph
  ( Edges
  , edgesOf
  , dependenciesOf
  , blockedBy
  , childrenOf
  , isReady
  , readyItems
  , Cycle (..)
  , cycleWitnessLimit
  , parentCycles
  , dependencyCycles
  , ancestorsOf
  , descendantsOf
  ) where

import Data.List (nub, sort, sortOn)
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

{- | A detected cycle, reported in bounded space: the first
'cycleWitnessLimit' nodes of a shortest cycle, the edge that closes that
cycle, and how many nodes it really has.
-}
data Cycle = Cycle
  { cycleWitness :: [Uuid]
  -- ^ The start of a shortest cycle, in traversal order, at most 'cycleWitnessLimit' long.
  , cycleClosing :: (Uuid, Uuid)
  -- ^ The edge from the cycle's last node back to its first.
  , cycleLength :: Int
  -- ^ The number of nodes in the cycle, witness truncation aside.
  }
  deriving (Eq, Show)

{- | How many nodes a cycle witness names. A long chain with one back edge
is the expected shape of a real roadmap graph, so the whole path is never
reported.
-}
cycleWitnessLimit :: Int
cycleWitnessLimit = 4

-- | Each @parent@ cycle, self-parents included.
parentCycles :: Store -> [Cycle]
parentCycles store = cyclesIn (Map.map (maybe [] pure . itemParent) (storeById store))

-- | Each dependency cycle, @blocks@ edges included.
dependencyCycles :: Store -> [Cycle]
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

{- | One bounded t'Cycle' per cyclic strongly connected component of an
adjacency map: every component of more than one vertex, plus self-loops.
Edges to vertices outside the map are ignored, since a dangling reference is
reported separately.

The witness is a shortest cycle through the component's lowest-numbered
vertex, which keeps the report bounded and deterministic without depending
on the size of the surrounding graph.
-}
cyclesIn :: Map Uuid [Uuid] -> [Cycle]
cyclesIn adjacency = sortOn cycleWitness (map witness (filter isCycle (components adjacency)))
 where
  isCycle [single] = single `elem` successors single
  isCycle component = length component > 1

  successors n = Map.findWithDefault [] n adjacency

  witness component =
    let start = minimum component
        rest = shortest (Set.fromList component) start
     in Cycle
          { cycleWitness = take cycleWitnessLimit (start : rest)
          , cycleClosing = (if null rest then start else last rest, start)
          , cycleLength = 1 + length rest
          }

  -- The vertices of a shortest cycle through @start@ after @start@ itself,
  -- in traversal order. A cyclic component always contains such a cycle, so
  -- the exhausted-queue case is unreachable; it degrades to the self-loop
  -- rather than failing.
  shortest component start = go (Map.singleton start start) [start]
   where
    go _ [] = []
    go parents (n : queue)
      | start `elem` reachable n = trace parents n
      | otherwise =
          let fresh = [m | m <- reachable n, not (Map.member m parents)]
           in go (foldr (`Map.insert` n) parents fresh) (queue <> fresh)

    reachable n = filter (`Set.member` component) (successors n)

    -- The path from @start@ down to @n@, @start@ excluded.
    trace parents n
      | n == start = []
      | otherwise = trace parents (Map.findWithDefault start n parents) <> [n]

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
