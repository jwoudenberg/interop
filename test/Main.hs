{-# LANGUAGE GADTs #-}
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
import Hedgehog hiding (test)
import qualified Hedgehog.Main
import qualified Interop.Diff
import qualified Interop.Wire as Wire
import qualified Interop.Wire.Flat as Flat
import qualified System.Directory as Directory
import qualified System.Exit
import qualified System.Process as Process
import qualified TypeChangeExamples.Base
import qualified TypeChangeExamples.V2.AddConstructor
import qualified TypeChangeExamples.V2.AddNonOptionalField
import qualified TypeChangeExamples.V2.AddOptionalField
import qualified TypeChangeExamples.V2.DropNonOptionalField
import qualified TypeChangeExamples.V2.DropOptionalField
import qualified TypeChangeExamples.V2.ModifyFieldType
import qualified TypeChangeExamples.V2.RemoveConstructor
import qualified TypeExamples.EnumType
import qualified TypeExamples.Float
import qualified TypeExamples.Int
import qualified TypeExamples.List
import qualified TypeExamples.NestedType
import qualified TypeExamples.Record
import qualified TypeExamples.RecursiveType
import qualified TypeExamples.Text
import qualified TypeExamples.Unit

main :: IO ()
main = do
  compileErrorExamples <- getCompileErrorExamples
  Hedgehog.Main.defaultMain
    [ checkParallel (Group "encode-decode roundtrip" (encodeDecodeRoundtripTest <$> exampleTypes)),
      checkParallel (Group "encoding" (encodingTest <$> exampleTypes)),
      checkParallel (Group "diff" (diffTest <$> changeExampleTypes)),
      checkParallel (Group "compile error" (compileErrorTest <$> compileErrorExamples))
    ]

encodeDecodeRoundtripTest :: ExampleType -> (PropertyName, Property)
encodeDecodeRoundtripTest (ExampleType path _ gen) =
  test (fromString path) $ do
    float <- forAll gen
    tripping float encode decode

encodingTest :: ExampleType -> (PropertyName, Property)
encodingTest (ExampleType path example _) =
  test1 (fromString path) $
    encodePretty example
      & ByteString.toStrict
      & Data.Text.Encoding.decodeUtf8
      & equalToCommentsInFile "JSON encoding of example value:" path

diffTest :: ChangeExampleType -> (PropertyName, Property)
diffTest (ChangeExampleType path changedType) =
  test1 (fromString path) $ do
    warnings <-
      typeChangeWarnings
        (Proxy :: Proxy TypeChangeExamples.Base.TestType)
        changedType
    equalToCommentsInFile "Warnings for this change from Base type:" path warnings

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
    equalToCommentsInFile "Compilation error:" examplePath (Text.pack actualError)

data ExampleType where
  ExampleType ::
    (Wire.Wire a, Show a, Eq a) =>
    { path :: FilePath,
      example :: a,
      gen :: Hedgehog.Gen a
    } ->
    ExampleType

exampleTypes :: [ExampleType]
exampleTypes =
  [ ExampleType "test/TypeExamples/EnumType.hs" (TypeExamples.EnumType.example) (TypeExamples.EnumType.gen),
    ExampleType "test/TypeExamples/Float.hs" (TypeExamples.Float.example) (TypeExamples.Float.gen),
    ExampleType "test/TypeExamples/Int.hs" (TypeExamples.Int.example) (TypeExamples.Int.gen),
    ExampleType "test/TypeExamples/List.hs" (TypeExamples.List.example) (TypeExamples.List.gen),
    ExampleType "test/TypeExamples/NestedType.hs" (TypeExamples.NestedType.example) (TypeExamples.NestedType.gen),
    ExampleType "test/TypeExamples/Record.hs" (TypeExamples.Record.example) (TypeExamples.Record.gen),
    ExampleType "test/TypeExamples/RecursiveType.hs" (TypeExamples.RecursiveType.example) (TypeExamples.RecursiveType.gen),
    ExampleType "test/TypeExamples/Text.hs" (TypeExamples.Text.example) (TypeExamples.Text.gen),
    ExampleType "test/TypeExamples/Unit.hs" (TypeExamples.Unit.example) (TypeExamples.Unit.gen)
  ]

data ChangeExampleType where
  ChangeExampleType ::
    Wire.Wire a =>
    FilePath ->
    Proxy a ->
    ChangeExampleType

changeExampleTypes :: [ChangeExampleType]
changeExampleTypes =
  [ ChangeExampleType
      "test/TypeChangeExamples/V2/AddConstructor.hs"
      (Proxy :: Proxy TypeChangeExamples.V2.AddConstructor.TestType),
    ChangeExampleType
      "test/TypeChangeExamples/V2/AddNonOptionalField.hs"
      (Proxy :: Proxy TypeChangeExamples.V2.AddNonOptionalField.TestType),
    ChangeExampleType
      "test/TypeChangeExamples/V2/AddOptionalField.hs"
      (Proxy :: Proxy TypeChangeExamples.V2.AddOptionalField.TestType),
    ChangeExampleType
      "test/TypeChangeExamples/V2/DropNonOptionalField.hs"
      (Proxy :: Proxy TypeChangeExamples.V2.DropNonOptionalField.TestType),
    ChangeExampleType
      "test/TypeChangeExamples/V2/DropOptionalField.hs"
      (Proxy :: Proxy TypeChangeExamples.V2.DropOptionalField.TestType),
    ChangeExampleType
      "test/TypeChangeExamples/V2/ModifyFieldType.hs"
      (Proxy :: Proxy TypeChangeExamples.V2.ModifyFieldType.TestType),
    ChangeExampleType
      "test/TypeChangeExamples/V2/RemoveConstructor.hs"
      (Proxy :: Proxy TypeChangeExamples.V2.RemoveConstructor.TestType)
  ]

getCompileErrorExamples :: IO [FilePath]
getCompileErrorExamples =
  let dir = "test/compile-error-examples/"
   in fmap (dir <>) <$> Directory.listDirectory dir

typeChangeWarnings :: (Wire.Wire a, Wire.Wire b) => Proxy a -> Proxy b -> PropertyT IO Text
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

equalToCommentsInFile :: Text -> FilePath -> Text -> PropertyT IO ()
equalToCommentsInFile header path actualUncommented = do
  contents <- evalIO $ Data.Text.IO.readFile path
  let actual =
        actualUncommented
          & Text.strip
          & Text.lines
          & (\lines' -> header : "" : lines')
          & fmap (\line -> Text.strip ("-- " <> line))
          & Text.unlines
  let (_code, expected) = Text.breakOn "-- " contents
  if Text.null expected
    then evalIO $ Data.Text.IO.appendFile path ("\n" <> actual)
    else ShowUnquoted (Text.strip actual) === ShowUnquoted (Text.strip expected)

newtype ShowUnquoted = ShowUnquoted Text
  deriving (Eq)

instance Show ShowUnquoted where
  show (ShowUnquoted text) = Text.unpack text
