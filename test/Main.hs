module Main where

import qualified Hedgehog
import qualified System.Exit

main :: IO ()
main = do
  success <- Hedgehog.checkParallel tests
  if success
    then System.Exit.exitSuccess
    else System.Exit.exitFailure

tests :: Hedgehog.Group
tests = Hedgehog.Group "tests" []
