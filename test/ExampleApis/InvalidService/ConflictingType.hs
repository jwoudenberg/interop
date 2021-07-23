module ExampleApis.InvalidService.ConflictingType where

import GHC.Generics (Generic)
import qualified Interop

data Transportation
  = Cycle
  | Drive
  | Fly
  deriving (Generic)

instance Interop.Wire Transportation
