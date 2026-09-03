{- | The @myque@ executable: a thin wrapper over "Myque.Cli", which
owns argument parsing, execution and the exit status.
-}
module Main (main) where

import Myque.Cli qualified as Cli

main :: IO ()
main = Cli.main
