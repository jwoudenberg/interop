module ExampleApis.AddFirstFieldToSecondConstructor.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Interop
import qualified Interop.Wire as Wire

data AddFirstFieldToSecondConstructorType
  = AddFirstFieldFirstConstructor
  | AddFirstFieldSecondConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire AddFirstFieldToSecondConstructorType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddFirstFieldToSecondConstructor" (\(req :: AddFirstFieldToSecondConstructorType) -> pure req)
  ]

gen :: Hedgehog.Gen AddFirstFieldToSecondConstructorType
gen = Gen.element [AddFirstFieldFirstConstructor, AddFirstFieldSecondConstructor]
