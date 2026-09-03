{- | An amortized O(1) purely functional FIFO queue (a \"banker's queue\").

The queue holds two lists: @front@ in dequeue order and @back@ in reverse
enqueue order. 'push' conses onto @back@, 'pop' unconses @front@, and when
@front@ would run dry it is refilled with @reverse back@.

The representation maintains the invariant

> null front  ==>  null back

so 'null' and 'peek' only ever inspect @front@ and stay O(1). Every element
is reversed at most once over its lifetime in the queue, so 'push' and 'pop'
are amortized O(1).
-}
module Myque.Queue
  ( Queue

    -- * Construction
  , empty
  , singleton
  , fromList

    -- * Modification
  , push
  , pop

    -- * Queries
  , peek
  , null
  , size
  , toList
  ) where

import Data.Function (on)
import Prelude hiding (null)
import Prelude qualified as P

{- | A FIFO queue of @a@.

'Eq' and 'Ord' compare the logical element sequence rather than the internal
split, so @'fromList' [1,2,3]@ equals every queue that dequeues @1,2,3@.
-}
data Queue a = Queue
  { front :: [a]
  -- ^ Elements in dequeue order.
  , back :: [a]
  -- ^ Elements in reverse enqueue order; empty whenever 'front' is empty.
  , count :: !Int
  -- ^ Cached length, so 'size' is O(1).
  }

-- | The empty queue.
empty :: Queue a
empty = Queue [] [] 0

-- | A queue holding exactly one element.
singleton :: a -> Queue a
singleton x = Queue [x] [] 1

-- | Build a queue that dequeues the elements in list order.
fromList :: [a] -> Queue a
fromList xs = Queue xs [] (P.length xs)

-- | Enqueue at the back. O(1).
push :: a -> Queue a -> Queue a
push x (Queue [] _ _) = Queue [x] [] 1
push x (Queue f b n) = Queue f (x : b) (n + 1)

{- | Dequeue from the front, returning the element and the remaining queue,
or 'Nothing' when the queue is empty. Amortized O(1), worst case O(n).
-}
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] _ _) = Nothing
pop (Queue (x : f) b n) = Just (x, refill f b (n - 1))

-- | Restore the @null front ==> null back@ invariant.
refill :: [a] -> [a] -> Int -> Queue a
refill [] b n = Queue (reverse b) [] n
refill f b n = Queue f b n

-- | The next element to be dequeued, without removing it. O(1).
peek :: Queue a -> Maybe a
peek (Queue [] _ _) = Nothing
peek (Queue (x : _) _ _) = Just x

-- | Whether the queue holds no elements. O(1).
null :: Queue a -> Bool
null (Queue f _ _) = P.null f

-- | Number of queued elements. O(1).
size :: Queue a -> Int
size = count

-- | Elements in dequeue order. O(n).
toList :: Queue a -> [a]
toList (Queue f b _) = f ++ reverse b

instance Foldable Queue where
  foldr f z q = P.foldr f z (toList q)
  foldMap f q = P.foldMap f (toList q)
  length = count

instance Functor Queue where
  fmap g (Queue f b n) = Queue (map g f) (map g b) n

instance Traversable Queue where
  traverse g q = fromList <$> traverse g (toList q)

instance Semigroup (Queue a) where
  l <> r
    | null l = r
    | null r = l
    | otherwise = Queue (toList l ++ front r) (back r) (count l + count r)

instance Monoid (Queue a) where
  mempty = empty

instance (Eq a) => Eq (Queue a) where
  (==) = (==) `on` toList

instance (Ord a) => Ord (Queue a) where
  compare = compare `on` toList

instance (Show a) => Show (Queue a) where
  showsPrec d q =
    showParen (d > 10) $
      showString "fromList " . showsPrec 11 (toList q)
