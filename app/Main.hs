{- | A tiny demo driver for "Myque.Queue": enqueue the command-line
arguments, then drain the queue in FIFO order.
-}
module Main (main) where

import Myque.Queue (Queue, empty, pop, push, size)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let items = if Prelude.null args then ["alpha", "beta", "gamma"] else args
      queue = foldl (flip push) (empty :: Queue String) items
  putStrLn $ "queued " <> show (size queue) <> " item(s)"
  drain queue

-- | Print each element in dequeue order.
drain :: Queue String -> IO ()
drain q = case pop q of
  Nothing -> pure ()
  Just (x, rest) -> putStrLn ("-> " <> x) >> drain rest
