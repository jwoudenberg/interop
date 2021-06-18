module ExampleApis.ModifyListToOptionalField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop
import qualified Interop.Wire as Wire

data TestType
  = OneConstructor Record
  | OtherConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire TestType

data Record = Record
  { field :: Int,
    optionalField :: Maybe Int,
    listField :: [Int]
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire Record

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint
      "echo"
      ( \req ->
          case req of
            OneConstructor _ ->
              pure
                ( OneConstructor
                    Record
                      { field = 1,
                        optionalField = Just 2,
                        listField = [1, 2, 3]
                      }
                )
            OtherConstructor ->
              pure OtherConstructor
      )
  ]

gen :: Hedgehog.Gen TestType
gen =
  Gen.choice
    [ do
        int <- Gen.int Range.exponentialBounded
        maybeInt <- Gen.maybe $ Gen.int Range.exponentialBounded
        listInt <- Gen.list (Range.linear 0 100) (Gen.int Range.exponentialBounded)
        pure $ OneConstructor (Record int maybeInt listInt),
      pure OtherConstructor
    ]
