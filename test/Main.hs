{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Char as Char
import Data.Function ((&))
import qualified Data.IORef as IORef
import Data.List (foldl')
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Data.Text.IO
import qualified ExampleApis.Api
import qualified ExampleTypeChanges.Base
import qualified ExampleTypeChanges.V2.AddConstructor
import qualified ExampleTypeChanges.V2.AddEndpoint
import qualified ExampleTypeChanges.V2.AddFirstField
import qualified ExampleTypeChanges.V2.AddListField
import qualified ExampleTypeChanges.V2.AddNonOptionalField
import qualified ExampleTypeChanges.V2.AddOptionalField
import qualified ExampleTypeChanges.V2.DropListField
import qualified ExampleTypeChanges.V2.DropNonOptionalField
import qualified ExampleTypeChanges.V2.DropOptionalField
import qualified ExampleTypeChanges.V2.ModifyFieldType
import qualified ExampleTypeChanges.V2.ModifyListToOptionalField
import qualified ExampleTypeChanges.V2.ModifyOptionalToListField
import qualified ExampleTypeChanges.V2.RemoveConstructor
import qualified ExampleTypes.EnumType
import qualified ExampleTypes.Float
import qualified ExampleTypes.Int
import qualified ExampleTypes.List
import qualified ExampleTypes.NestedRecord
import qualified ExampleTypes.NestedType
import qualified ExampleTypes.Record
import qualified ExampleTypes.RecursiveType
import qualified ExampleTypes.Text
import qualified ExampleTypes.Unit
import qualified GHC
import qualified GHC.Paths
import Hedgehog hiding (test)
import qualified Hedgehog.Main
import qualified Interop
import qualified Interop.Diff
import qualified Interop.Ruby
import qualified Interop.Wire as Wire
import qualified Network.Wai.Handler.Warp as Warp
import qualified Outputable
import qualified System.Directory as Directory
import qualified System.Exit
import qualified System.FilePath as FilePath
import qualified System.IO
import qualified System.Process as Process

main :: IO ()
main = do
  compileErrorExamples <- getCompileErrorExamples
  Hedgehog.Main.defaultMain
    [ checkParallel (Group "encode-decode roundtrip" (encodeDecodeRoundtripTest <$> exampleTypes)),
      checkParallel (Group "encoding" (encodingTest <$> exampleTypes)),
      checkParallel (Group "diff" (diffTest <$> changeExampleTypes)),
      checkParallel (Group "compile error" (compileErrorTest <$> compileErrorExamples)),
      checkParallel (Group "backwards-compatible decoding" backwardsCompatibleDecodingTests),
      checkParallel (Group "ruby client generation" rubyClientGenerationTests),
      checkParallel (Group "generated ruby code" generatedRubyCodeTests)
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

diffTest :: ChangeExample -> (PropertyName, Property)
diffTest (ChangeExample name changedService) =
  let path = "test/ExampleTypeChanges/V2/" <> name <> ".hs"
   in test1 (fromString path) $ do
        warnings <-
          typeChangeWarnings
            ExampleTypeChanges.Base.service
            changedService
        equalToCommentsInFile "Warnings for this change from Base type:" path warnings

backwardsCompatibleDecodingTests :: [(PropertyName, Property)]
backwardsCompatibleDecodingTests =
  [ test "Server with new constructor can still decode old types" $ do
      oldType <- forAll ExampleTypeChanges.Base.gen
      let encoded = encode oldType
      (_ :: ExampleTypeChanges.V2.AddConstructor.TestType) <- evalEither (decode encoded)
      pure (),
    test "Server with new first field can still serve old types" $ do
      oldType <- forAll ExampleTypeChanges.Base.gen
      let encoded = encode oldType
      (_ :: ExampleTypeChanges.V2.AddFirstField.TestType) <- evalEither (decode encoded)
      pure (),
    test "Server with new optional field can still decode old types" $ do
      oldType <- forAll ExampleTypeChanges.Base.gen
      let encoded = encode oldType
      (_ :: ExampleTypeChanges.V2.AddOptionalField.TestType) <- evalEither (decode encoded)
      pure (),
    test "Server with new list field can still decode old types" $ do
      oldType <- forAll ExampleTypeChanges.Base.gen
      let encoded = encode oldType
      (_ :: ExampleTypeChanges.V2.AddListField.TestType) <- evalEither (decode encoded)
      pure (),
    test "Server which dropped a non-optional field can still decode old types" $ do
      oldType <- forAll ExampleTypeChanges.Base.gen
      let encoded = encode oldType
      (_ :: ExampleTypeChanges.V2.DropNonOptionalField.TestType) <- evalEither (decode encoded)
      pure (),
    test "Server which dropped an optional field can still decode old types" $ do
      oldType <- forAll ExampleTypeChanges.Base.gen
      let encoded = encode oldType
      (_ :: ExampleTypeChanges.V2.DropOptionalField.TestType) <- evalEither (decode encoded)
      pure (),
    test "Server which dropped a list field can still decode old types" $ do
      oldType <- forAll ExampleTypeChanges.Base.gen
      let encoded = encode oldType
      (_ :: ExampleTypeChanges.V2.DropListField.TestType) <- evalEither (decode encoded)
      pure ()
  ]

rubyClientGenerationTests :: [(PropertyName, Property)]
rubyClientGenerationTests =
  [ test1 "ExampleApi" $ do
      (path, h) <- evalIO $ System.IO.openTempFile "/tmp" "interop-tests-ruby-generation.rb"
      evalIO $ System.IO.hClose h
      evalIO $ Interop.Ruby.generate path ["Apis", "ExampleApi"] ExampleApis.Api.service
      generated <- evalIO $ Data.Text.IO.readFile path
      equalToContentsOfFile "test/ruby-tests/apis/example_api.rb" generated
  ]
    <> ( changeExampleTypes
           & fmap
             ( \(ChangeExample name service) ->
                 test1 (fromString name) $ do
                   (path, h) <- evalIO $ System.IO.openTempFile "/tmp" "interop-tests-ruby-generation.rb"
                   evalIO $ System.IO.hClose h
                   evalIO $ Interop.Ruby.generate path ["Apis", "V2", Text.pack name] service
                   generated <- evalIO $ Data.Text.IO.readFile path
                   equalToContentsOfFile ("test/ruby-tests/apis/v2/" <> toSnakeCase name <> ".rb") generated
             )
       )

generatedRubyCodeTests :: [(PropertyName, Property)]
generatedRubyCodeTests =
  [ test1 "Generated ruby code" $ do
      let app =
            ExampleApis.Api.endpoints
              ++ ExampleTypeChanges.Base.endpoints
              & Interop.service
              & either (error . show) id
              & Interop.wai
      (exitCode, stdout, stderr) <-
        evalIO $
          Warp.testWithApplication (pure app) $ \port -> do
            let proc =
                  (Process.proc "ruby" ["test.rb"])
                    { Process.env = Just [("PORT", show port)],
                      Process.cwd = Just "test/ruby-tests"
                    }
            Process.readCreateProcessWithExitCode proc ""
      case exitCode of
        System.Exit.ExitSuccess -> pure ()
        System.Exit.ExitFailure _ -> fail (stderr <> stdout)
  ]

-- | We'd like to test our custom compilation errors for Wire instances, and
-- this puts us in a tricky spot. We can't well put examples of Wire instances
-- for unsupported types in our regular source tree, or our test suite won't
-- compile!
--
-- Instead we put bad examples in the example-compile-errors directory, which
-- we don't list as a source directory so Cabal won't attempt to compile it.
-- Then we call GHC as a library to compile those module's and intercept the
-- compilation errors this results in.
compileErrorTest :: FilePath -> (PropertyName, Property)
compileErrorTest examplePath =
  test1 (fromString examplePath) $ do
    errs <-
      evalIO $
        GHC.runGhc (Just GHC.Paths.libdir) $ do
          logRef <- liftIO $ IORef.newIORef []
          dflags <- GHC.getSessionDynFlags
          let modifiedDflags =
                dflags
                  { -- Make GHC aware of the library source code.
                    GHC.importPaths = "src/" : GHC.importPaths dflags,
                    -- Don't generate .hi or .o files of compiled code. These
                    -- aren't necessary (we only want a compilation check), slow
                    -- down tests, and can result in test flakiness when
                    -- parallel tests compiling the same module try to write the
                    -- same files.
                    GHC.hscTarget = GHC.HscNothing,
                    -- Prevent GHC from logging to stdout. Use our own logic.
                    GHC.log_action = \dflags' _ severity _ _ msg ->
                      case severity of
                        GHC.SevFatal -> fail $ Outputable.showSDoc dflags' msg
                        GHC.SevError -> IORef.modifyIORef' logRef (Outputable.showSDoc dflags' msg :)
                        _ -> return ()
                  }
          _ <- GHC.setSessionDynFlags modifiedDflags
          target <- GHC.guessTarget examplePath Nothing
          GHC.setTargets [target]
          _ <- GHC.load GHC.LoadAllTargets
          liftIO $ IORef.readIORef logRef
    fmap Text.pack errs
      & Text.intercalate "\n\n"
      & equalToCommentsInFile "Compilation error:" examplePath

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
  [ ExampleType "test/ExampleTypes/EnumType.hs" (ExampleTypes.EnumType.example) (ExampleTypes.EnumType.gen),
    ExampleType "test/ExampleTypes/Float.hs" (ExampleTypes.Float.example) (ExampleTypes.Float.gen),
    ExampleType "test/ExampleTypes/Int.hs" (ExampleTypes.Int.example) (ExampleTypes.Int.gen),
    ExampleType "test/ExampleTypes/List.hs" (ExampleTypes.List.example) (ExampleTypes.List.gen),
    ExampleType "test/ExampleTypes/NestedType.hs" (ExampleTypes.NestedType.example) (ExampleTypes.NestedType.gen),
    ExampleType "test/ExampleTypes/NestedRecord.hs" (ExampleTypes.NestedRecord.example) (ExampleTypes.NestedRecord.gen),
    ExampleType "test/ExampleTypes/Record.hs" (ExampleTypes.Record.example) (ExampleTypes.Record.gen),
    ExampleType "test/ExampleTypes/RecursiveType.hs" (ExampleTypes.RecursiveType.example) (ExampleTypes.RecursiveType.gen),
    ExampleType "test/ExampleTypes/Text.hs" (ExampleTypes.Text.example) (ExampleTypes.Text.gen),
    ExampleType "test/ExampleTypes/Unit.hs" (ExampleTypes.Unit.example) (ExampleTypes.Unit.gen)
  ]

data ChangeExample where
  ChangeExample ::
    FilePath ->
    Interop.Service a ->
    ChangeExample

changeExampleTypes :: [ChangeExample]
changeExampleTypes =
  [ ChangeExample
      "AddConstructor"
      ExampleTypeChanges.V2.AddConstructor.service,
    ChangeExample
      "AddEndpoint"
      ExampleTypeChanges.V2.AddEndpoint.service,
    ChangeExample
      "AddFirstField"
      ExampleTypeChanges.V2.AddFirstField.service,
    ChangeExample
      "AddNonOptionalField"
      ExampleTypeChanges.V2.AddNonOptionalField.service,
    ChangeExample
      "AddOptionalField"
      ExampleTypeChanges.V2.AddOptionalField.service,
    ChangeExample
      "AddListField"
      ExampleTypeChanges.V2.AddListField.service,
    ChangeExample
      "DropNonOptionalField"
      ExampleTypeChanges.V2.DropNonOptionalField.service,
    ChangeExample
      "DropOptionalField"
      ExampleTypeChanges.V2.DropOptionalField.service,
    ChangeExample
      "DropListField"
      ExampleTypeChanges.V2.DropListField.service,
    ChangeExample
      "ModifyListToOptionalField"
      ExampleTypeChanges.V2.ModifyListToOptionalField.service,
    ChangeExample
      "ModifyOptionalToListField"
      ExampleTypeChanges.V2.ModifyOptionalToListField.service,
    ChangeExample
      "ModifyFieldType"
      ExampleTypeChanges.V2.ModifyFieldType.service,
    ChangeExample
      "RemoveConstructor"
      ExampleTypeChanges.V2.RemoveConstructor.service
  ]

getCompileErrorExamples :: IO [FilePath]
getCompileErrorExamples =
  let dir = "test/example-compile-errors/"
   in fmap (dir <>) <$> Directory.listDirectory dir

typeChangeWarnings :: Interop.Service a -> Interop.Service b -> PropertyT IO Text
typeChangeWarnings server client =
  Interop.Diff.compatible
    server
    client
    & pure

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

-- | Checks the passed in Text value is equal to the contents of a Haskell-type
-- comment section in the provided file. If the file does not contain a comment
-- section it is added.
--
-- This allows tests where input and output live next to each other in example
-- files, and have those files serve as verified documentation.
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

-- | Checks the passed in Text value is equal to the contents of a file.
-- If the file does not exist it is created.
equalToContentsOfFile :: FilePath -> Text -> PropertyT IO ()
equalToContentsOfFile path actual = do
  evalIO $ Directory.createDirectoryIfMissing True (FilePath.takeDirectory path)
  exists <- evalIO $ Directory.doesFileExist path
  if exists
    then do
      expected <- evalIO $ Data.Text.IO.readFile path
      ShowUnquoted (Text.strip actual) === ShowUnquoted (Text.strip expected)
    else evalIO $ Data.Text.IO.writeFile path actual

-- | Has a `Show` instance that prints contained text without any escape
-- characters.
newtype ShowUnquoted = ShowUnquoted Text
  deriving (Eq)

instance Show ShowUnquoted where
  show (ShowUnquoted text) = Text.unpack text

toSnakeCase :: String -> String
toSnakeCase string =
  foldl'
    ( \acc char ->
        case (acc, Char.isUpper char) of
          ([], _) -> [Char.toLower char]
          (_, True) -> Char.toLower char : '_' : acc
          (_, False) -> char : acc
    )
    []
    string
    & reverse
