-- | This module isn't part of the source code, but only used as a tool during
-- development in combination with ghcid.
--
-- To use it, invoke ghcid like so:
-- > ghcid --test Devel.main
module Devel (main) where

import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Ruby

main :: IO ()
main =
  case Interop.service endpoints of
    Left err -> print err
    Right service ->
      Interop.Ruby.generate "/dev/stdout" service

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.Endpoint "double" (\(Number x) -> pure (2 * x))
  ]

data Number = Number {n :: Int}
  deriving (Generic)

instance Interop.Wire Number
