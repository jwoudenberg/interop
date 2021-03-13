module TypeChangeExamples.Base where

import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop.Wire as Wire

data TestType
  = OneConstructor Record
  | OtherConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire TestType

data Record = Record
  { field :: Int,
    optionalField :: Maybe Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire Record

gen :: Hedgehog.Gen TestType
gen =
  Gen.choice
    [ do
        int <- Gen.int Range.exponentialBounded
        maybeInt <- Gen.maybe $ Gen.int Range.exponentialBounded
        pure $ OneConstructor (Record int maybeInt),
      pure OtherConstructor
    ]
