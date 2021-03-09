module Main where

import Control.Monad ((<=<))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog hiding (test)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main
import qualified Hedgehog.Range as Range
import qualified Interop.Wire as Wire

main :: IO ()
main =
  Hedgehog.Main.defaultMain
    [ checkParallel encodeDecodeRoundtripTests
    ]

encodeDecodeRoundtripTests :: Group
encodeDecodeRoundtripTests =
  Group
    "encode-decode roundtrip"
    [ test "Int" $ do
        int <- forAll $ Gen.int Range.exponentialBounded
        tripping int encode decode,
      test "Float" $ do
        float <- forAll $ Gen.float (Range.exponentialFloat 0 1000)
        tripping float encode decode,
      test "Text" $ do
        text <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
        tripping text encode decode,
      test "Unit" $ tripping () encode decode,
      test "List" $ do
        list <- forAll $ Gen.list (Range.linear 0 100) (Gen.int Range.exponentialBounded)
        tripping list encode decode,
      test "Record" $ do
        record <- forAll genRecord
        tripping record encode decode,
      test "EnumType" $ do
        record <- forAll genEnumType
        tripping record encode decode
    ]

data Record = Record
  { oneField :: Int,
    otherField :: Text
  }
  deriving (Eq, Generic, Show)

instance Wire.Wire Record

genRecord :: Gen Record
genRecord =
  Record
    <$> Gen.int Range.exponentialBounded
    <*> Gen.text (Range.linear 0 100) Gen.unicode

data EnumType
  = OneConstructor
  | OtherConstructor
  deriving (Eq, Generic, Show)

instance Wire.Wire EnumType

genEnumType :: Gen EnumType
genEnumType = Gen.element [OneConstructor, OtherConstructor]

encode :: Wire.Wire a => a -> ByteString
encode = Encoding.encodingToLazyByteString . Wire.encode

decode :: Wire.Wire a => ByteString -> Either String a
decode = Aeson.parseEither Wire.decode <=< Aeson.eitherDecode

test :: PropertyName -> PropertyT IO () -> (PropertyName, Property)
test description prop = (description, property prop)
