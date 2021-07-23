module ExampleApis.AddFirstFieldToSecondConstructor.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Interop
import qualified Interop.Wire as Wire

data AddFirstFieldType
  = AddFirstFieldFirstConstructor
  | AddFirstFieldSecondConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire AddFirstFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddFirstField" (\(req :: AddFirstFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen AddFirstFieldType
gen = Gen.element [AddFirstFieldFirstConstructor, AddFirstFieldSecondConstructor]
