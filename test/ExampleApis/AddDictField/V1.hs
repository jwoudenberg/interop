module ExampleApis.AddDictField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop
import qualified Interop.Wire as Wire

data AddDictFieldType = AddDictFieldType
  { field :: Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire AddDictFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddDictField" (\(req :: AddDictFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen AddDictFieldType
gen = do
  int <- Gen.int Range.exponentialBounded
  pure $ AddDictFieldType int
