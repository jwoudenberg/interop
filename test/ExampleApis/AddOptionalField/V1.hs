module ExampleApis.AddOptionalField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop
import qualified Interop.Wire as Wire

data AddOptionalFieldType = AddOptionalFieldType
  { field :: Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire AddOptionalFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddOptionalField" (\(req :: AddOptionalFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen AddOptionalFieldType
gen = do
  int <- Gen.int Range.exponentialBounded
  pure $ AddOptionalFieldType int
