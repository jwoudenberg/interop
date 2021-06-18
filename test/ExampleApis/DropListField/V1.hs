module ExampleApis.DropListField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop
import qualified Interop.Wire as Wire

data DropListFieldType = DropListFieldType
  { field :: Int,
    listField :: [Int]
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire DropListFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "DropListField" (\(req :: DropListFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen DropListFieldType
gen = do
  int <- Gen.int Range.exponentialBounded
  listInt <- Gen.list (Range.linear 0 100) (Gen.int Range.exponentialBounded)
  pure $ DropListFieldType int listInt
