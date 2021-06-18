module ExampleApis.ModifyOptionalToListField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop
import qualified Interop.Wire as Wire

data ModifyOptionalToListFieldType = ModifyOptionalToListFieldType
  { optionalField :: Maybe Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire ModifyOptionalToListFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "ModifyOptionalToListField" (\(req :: ModifyOptionalToListFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen ModifyOptionalToListFieldType
gen = do
  maybeInt <- Gen.maybe $ Gen.int Range.exponentialBounded
  pure $ ModifyOptionalToListFieldType maybeInt
