-- The Monoid law tests must evaluate the very expressions HLint wants to
-- simplify away, so the identity hints are false positives in this file.
{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

-- | Behavioural tests for "Myque.Queue".
module Main (main) where

import Data.Foldable qualified as F
import Data.List (sort)
import Myque.Queue
import Test.Hspec
import Prelude hiding (null)
import Prelude qualified as P

-- | Drain a queue completely, collecting elements in dequeue order.
drain :: Queue a -> [a]
drain q = case pop q of
  Nothing -> []
  Just (x, rest) -> x : drain rest

{- | Build a queue only through 'push', so the internal front/back split is
exercised rather than the flat 'fromList' representation.
-}
pushed :: [a] -> Queue a
pushed = foldl (flip push) empty

main :: IO ()
main = hspec $ do
  describe "ordering" $ do
    it "dequeues pushed elements first-in first-out" $
      drain (pushed [1 :: Int .. 5]) `shouldBe` [1 .. 5]

    it "survives interleaved pushes and pops" $ do
      -- push 1,2,3; pop 1; push 4,5; pop the rest.
      let q0 = pushed [1 :: Int, 2, 3]
      case pop q0 of
        Nothing -> expectationFailure "pop returned Nothing on a non-empty queue"
        Just (x, q1) -> do
          x `shouldBe` 1
          drain (push 5 (push 4 q1)) `shouldBe` [2, 3, 4, 5]

    it "refills the front from the back exactly once per element" $ do
      -- Draining to empty and refilling must not resurrect stale elements.
      let q = pushed [1 :: Int, 2]
          emptied = P.foldr (\_ acc -> maybe acc snd (pop acc)) q [1 :: Int, 2]
      null emptied `shouldBe` True
      drain (push 9 emptied) `shouldBe` [9]

  describe "empty queue" $ do
    it "pops to Nothing" $
      pop (empty :: Queue Int) `shouldBe` Nothing

    it "peeks to Nothing" $
      peek (empty :: Queue Int) `shouldBe` Nothing

    it "is null and has size 0" $ do
      null (empty :: Queue Int) `shouldBe` True
      size (empty :: Queue Int) `shouldBe` 0

  describe "peek" $ do
    it "returns the next element without removing it" $ do
      let q = pushed [7 :: Int, 8]
      peek q `shouldBe` Just 7
      size q `shouldBe` 2

    it "agrees with pop" $ do
      let q = pushed "abc"
      peek q `shouldBe` fmap fst (pop q)

  describe "size" $ do
    it "counts pushes" $
      size (pushed [1 :: Int .. 4]) `shouldBe` 4

    it "decreases by one per pop" $ do
      let q = pushed [1 :: Int .. 4]
      fmap (size . snd) (pop q) `shouldBe` Just 3

    it "stays in step with toList through mixed operations" $ do
      let q = push 4 (maybe empty snd (pop (pushed [1 :: Int, 2, 3])))
      size q `shouldBe` P.length (toList q)

  describe "toList" $ do
    it "is the dequeue order" $ do
      let q = pushed [1 :: Int .. 5]
      toList q `shouldBe` drain q

    it "round-trips through fromList" $
      toList (fromList [1 :: Int .. 5]) `shouldBe` [1 .. 5]

    it "matches Foldable.toList" $ do
      let q = push 3 (maybe empty snd (pop (pushed [1 :: Int, 2])))
      F.toList q `shouldBe` toList q

  describe "singleton" $
    it "holds exactly one element" $ do
      let q = singleton 'x'
      size q `shouldBe` 1
      drain q `shouldBe` "x"

  describe "Eq" $ do
    it "ignores the internal front/back split" $
      -- One queue is all-front, the other has elements sitting in the back.
      pushed [1 :: Int, 2, 3] `shouldBe` fromList [1, 2, 3]

    it "distinguishes different orders" $
      pushed [1 :: Int, 2] `shouldNotBe` pushed [2 :: Int, 1]

  describe "Ord" $
    it "orders lexicographically by dequeue order" $
      sort [pushed [2 :: Int], pushed [1 :: Int, 9], empty]
        `shouldBe` [empty, pushed [1, 9], pushed [2]]

  describe "Semigroup/Monoid" $ do
    it "concatenates in dequeue order" $
      toList (pushed [1 :: Int, 2] <> pushed [3 :: Int, 4]) `shouldBe` [1, 2, 3, 4]

    it "sums sizes" $
      size (pushed [1 :: Int, 2] <> pushed [3 :: Int, 4]) `shouldBe` 4

    it "has mempty as identity" $ do
      let q = pushed [1 :: Int, 2, 3]
      (mempty <> q) `shouldBe` q
      (q <> mempty) `shouldBe` q

    it "is associative across a back-loaded queue" $ do
      let a = pushed [1 :: Int, 2]
          b = push 4 (maybe empty snd (pop (pushed [3 :: Int, 9])))
          c = pushed [5 :: Int]
      toList ((a <> b) <> c) `shouldBe` toList (a <> (b <> c))

  describe "Functor" $
    it "preserves order and size" $ do
      let q = fmap (* 2) (push 3 (maybe empty snd (pop (pushed [1 :: Int, 2]))))
      toList q `shouldBe` [4, 6]
      size q `shouldBe` 2

  describe "Traversable" $ do
    it "sequences effects in dequeue order" $
      traverse Just (pushed [1 :: Int, 2, 3]) `shouldBe` Just (fromList [1, 2, 3])

    it "short-circuits on failure" $
      traverse (\x -> if x > 2 then Nothing else Just x) (pushed [1 :: Int, 3])
        `shouldBe` Nothing

  describe "Foldable" $ do
    it "folds in dequeue order" $
      foldr (:) [] (pushed [1 :: Int, 2, 3]) `shouldBe` [1, 2, 3]

    it "reports length without draining" $
      P.length (pushed [1 :: Int .. 6]) `shouldBe` 6

  describe "Show" $ do
    it "renders as a fromList call" $
      show (pushed [1 :: Int, 2]) `shouldBe` "fromList [1,2]"

    it "parenthesises when nested" $
      show (Just (pushed [1 :: Int])) `shouldBe` "Just (fromList [1])"
