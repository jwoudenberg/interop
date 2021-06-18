module ExampleApis.RemoveConstructor.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Interop
import qualified Interop.Wire as Wire

data RemoveConstructorType
  = KeepThisConstructor
  | AlsoKeepThisConstructor
  | RemoveThisConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire RemoveConstructorType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "RemoveConstructor" (\(req :: RemoveConstructorType) -> pure req)
  ]

gen :: Hedgehog.Gen RemoveConstructorType
gen = Gen.element [KeepThisConstructor, AlsoKeepThisConstructor, RemoveThisConstructor]
