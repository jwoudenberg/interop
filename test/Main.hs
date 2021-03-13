{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad ((<=<))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Data.Text.IO
import GHC.Generics (Generic)
import Hedgehog hiding (test)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main
import qualified Hedgehog.Range as Range
import qualified Interop.Diff
import qualified Interop.Wire as Wire
import qualified Interop.Wire.Flat as Flat
import qualified System.Directory as Directory
import qualified System.Exit
import qualified System.Process as Process
import qualified TestTypes.Base
import qualified TestTypes.V2.AddConstructor
import qualified TestTypes.V2.AddNonOptionalField
import qualified TestTypes.V2.AddOptionalField
import qualified TestTypes.V2.DropNonOptionalField
import qualified TestTypes.V2.DropOptionalField
import qualified TestTypes.V2.ModifyFieldType
import qualified TestTypes.V2.RemoveConstructor

main :: IO ()
main = do
  compileErrorExamples <- getCompileErrorExamples
  Hedgehog.Main.defaultMain
    [ checkParallel encodeDecodeRoundtripTests,
      checkParallel encodingTests,
      checkParallel diffTests,
      checkParallel (Group "comile error" (compileErrorTest <$> compileErrorExamples))
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
        tripping record encode decode,
      test "NestedType" $ do
        record <- forAll genNestedType
        tripping record encode decode,
      test "RecursiveType" $ do
        record <- forAll genRecursiveType
        tripping record encode decode
    ]

encodingTests :: Group
encodingTests =
  Group
    "encoding"
    [ test1 "Int" $
        encodePretty (4 :: Int)
          & equalToFile "test/golden-results/encoded-int.json",
      test1 "Float" $
        encodePretty (4.1 :: Float)
          & equalToFile "test/golden-results/encoded-float.json",
      test1 "Text" $
        encodePretty ("Hi!" :: Text)
          & equalToFile "test/golden-results/encoded-text.json",
      test1 "Unit" $
        encodePretty ()
          & equalToFile "test/golden-results/encoded-unit.json",
      test1 "List" $
        encodePretty [1, 2, 3 :: Int]
          & equalToFile "test/golden-results/encoded-list.json",
      test1 "Record" $
        encodePretty (Record 2 "Hi!")
          & equalToFile "test/golden-results/encoded-record.json",
      test1 "EnumType" $
        encodePretty OneConstructor
          & equalToFile "test/golden-results/encoded-enum-type.json",
      test1 "NestedType" $
        encodePretty (NestedType (Record 2 "Hi!"))
          & equalToFile "test/golden-results/encoded-nested-type.json",
      test1 "RecursiveType" $
        encodePretty (RecursiveType (Just (RecursiveType Nothing)))
          & equalToFile "test/golden-results/encoded-recursive-type.json"
    ]

diffTests :: Group
diffTests =
  Group
    "diff"
    [ test1 "add constructor" $ do
        warnings <-
          typeChangeWarnings
            (Proxy :: Proxy TestTypes.Base.TestType)
            (Proxy :: Proxy TestTypes.V2.AddConstructor.TestType)
        equalToFile "test/golden-results/warnings-add-constructor.txt" warnings,
      test1 "add non optional field" $ do
        warnings <-
          typeChangeWarnings
            (Proxy :: Proxy TestTypes.Base.TestType)
            (Proxy :: Proxy TestTypes.V2.AddNonOptionalField.TestType)
        equalToFile "test/golden-results/warnings-add-non-optional-field.txt" warnings,
      test1 "add optional field" $ do
        warnings <-
          typeChangeWarnings
            (Proxy :: Proxy TestTypes.Base.TestType)
            (Proxy :: Proxy TestTypes.V2.AddOptionalField.TestType)
        equalToFile "test/golden-results/warnings-add-optional-field.txt" warnings,
      test1 "drop non optional field" $ do
        warnings <-
          typeChangeWarnings
            (Proxy :: Proxy TestTypes.Base.TestType)
            (Proxy :: Proxy TestTypes.V2.DropNonOptionalField.TestType)
        equalToFile "test/golden-results/warnings-drop-non-optional-field.txt" warnings,
      test1 "drop optional field" $ do
        warnings <-
          typeChangeWarnings
            (Proxy :: Proxy TestTypes.Base.TestType)
            (Proxy :: Proxy TestTypes.V2.DropOptionalField.TestType)
        equalToFile "test/golden-results/warnings-drop-optional-field.txt" warnings,
      test1 "modify field type" $ do
        warnings <-
          typeChangeWarnings
            (Proxy :: Proxy TestTypes.Base.TestType)
            (Proxy :: Proxy TestTypes.V2.ModifyFieldType.TestType)
        equalToFile "test/golden-results/warnings-modify-field-type.txt" warnings,
      test1 "remove constructor" $ do
        warnings <-
          typeChangeWarnings
            (Proxy :: Proxy TestTypes.Base.TestType)
            (Proxy :: Proxy TestTypes.V2.RemoveConstructor.TestType)
        equalToFile "test/golden-results/warnings-remove-constructor.txt" warnings
    ]

compileErrorTest :: FilePath -> (PropertyName, Property)
compileErrorTest examplePath =
  test1 (fromString examplePath) $ do
    let proc =
          ( Process.proc
              "runghc"
              ["../" <> examplePath]
          )
            { Process.cwd = Just "src"
            }
    (exitCode, _, actualError) <- evalIO $ Process.readCreateProcessWithExitCode proc ""
    case exitCode of
      System.Exit.ExitSuccess -> fail "Expected process to fail withc compiler error, but it didn't"
      System.Exit.ExitFailure _ -> pure ()
    exampleContents <- evalIO $ Data.Text.IO.readFile examplePath
    let actualCommentedError =
          Text.pack actualError
            & Text.strip
            & Text.lines
            & fmap (\line -> Text.strip ("-- " <> line))
            & Text.unlines
    let (_code, expectedCommentedError) = Text.breakOn "-- " exampleContents
    if Text.null expectedCommentedError
      then evalIO $ Data.Text.IO.appendFile examplePath actualCommentedError
      else ShowUnquoted (Text.strip actualCommentedError) === ShowUnquoted (Text.strip expectedCommentedError)

newtype ShowUnquoted = ShowUnquoted Text
  deriving (Eq)

instance Show ShowUnquoted where
  show (ShowUnquoted text) = Text.unpack text

getCompileErrorExamples :: IO [FilePath]
getCompileErrorExamples =
  let dir = "test/compile-error-examples/"
   in fmap (dir <>) <$> Directory.listDirectory dir

typeChangeWarnings :: (Wire.Wire a, Wire.Wire b) => Proxy a -> Proxy b -> PropertyT IO ByteString
typeChangeWarnings before after = do
  flatBefore <- diffableType (Wire.type_ before)
  flatAfter <- diffableType (Wire.type_ after)
  Interop.Diff.diffType
    flatBefore
    flatAfter
    & Interop.Diff.checkBackwardsCompatibility
    & fmap Interop.Diff.warningToText
    & ( \case
          [] -> "No warnings."
          warnings -> Text.intercalate "\n\n" warnings
      )
    & Data.Text.Encoding.encodeUtf8
    & ByteString.fromStrict
    & pure

diffableType :: Wire.WireType -> PropertyT IO (Map.Map Text Flat.CustomType, Flat.Type)
diffableType wireType = do
  customTypes <- evalEither $ Flat.customTypes wireType
  pure
    ( fmap
        (\customType -> (Flat.typeName customType, customType))
        customTypes
        & Map.fromList,
      Flat.fromFieldType wireType
    )

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

data NestedType = NestedType Record
  deriving (Eq, Generic, Show)

instance Wire.Wire NestedType

genNestedType :: Gen NestedType
genNestedType = NestedType <$> genRecord

data RecursiveType = RecursiveType
  { recursiveField :: Maybe RecursiveType
  }
  deriving (Eq, Generic, Show)

instance Wire.Wire RecursiveType

genRecursiveType :: Gen RecursiveType
genRecursiveType =
  Gen.recursive
    Gen.choice
    [pure Nothing]
    [Just <$> genRecursiveType]
    & fmap RecursiveType

encode :: Wire.Wire a => a -> ByteString
encode = Encoding.encodingToLazyByteString . Wire.encode

decode :: Wire.Wire a => ByteString -> Either String a
decode = Aeson.parseEither Wire.decode <=< Aeson.eitherDecode

test :: PropertyName -> PropertyT IO () -> (PropertyName, Property)
test description prop = (description, property prop)

test1 :: PropertyName -> PropertyT IO () -> (PropertyName, Property)
test1 description prop = (description, withTests 1 (property prop))

encodePretty :: Wire.Wire a => a -> ByteString
encodePretty val =
  let compactEncoded = encode val
   in case Aeson.decode compactEncoded of
        Nothing -> compactEncoded
        Just (decoded :: Aeson.Value) -> Data.Aeson.Encode.Pretty.encodePretty decoded

equalToFile :: FilePath -> ByteString -> PropertyT IO ()
equalToFile path actual = do
  fileExists <- evalIO (Directory.doesFileExist path)
  if fileExists
    then do
      expected <- evalIO (ByteString.readFile path)
      expected === actual
    else evalIO (ByteString.writeFile path actual)
