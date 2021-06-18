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
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Data.Text.IO
import qualified ExampleApiChanges.AddConstructor.V1
import qualified ExampleApiChanges.AddConstructor.V2
import qualified ExampleApiChanges.AddDictField.V1
import qualified ExampleApiChanges.AddDictField.V2
import qualified ExampleApiChanges.AddEndpoint.V1
import qualified ExampleApiChanges.AddEndpoint.V2
import qualified ExampleApiChanges.AddFirstField.V1
import qualified ExampleApiChanges.AddFirstField.V2
import qualified ExampleApiChanges.AddListField.V1
import qualified ExampleApiChanges.AddListField.V2
import qualified ExampleApiChanges.AddNonOptionalField.V1
import qualified ExampleApiChanges.AddNonOptionalField.V2
import qualified ExampleApiChanges.AddOptionalField.V1
import qualified ExampleApiChanges.AddOptionalField.V2
import qualified ExampleApiChanges.DropAllFields.V1
import qualified ExampleApiChanges.DropAllFields.V2
import qualified ExampleApiChanges.DropListField.V1
import qualified ExampleApiChanges.DropListField.V2
import qualified ExampleApiChanges.DropNonOptionalField.V1
import qualified ExampleApiChanges.DropNonOptionalField.V2
import qualified ExampleApiChanges.DropOptionalField.V1
import qualified ExampleApiChanges.DropOptionalField.V2
import qualified ExampleApiChanges.ModifyFieldType.V1
import qualified ExampleApiChanges.ModifyFieldType.V2
import qualified ExampleApiChanges.ModifyListToOptionalField.V1
import qualified ExampleApiChanges.ModifyListToOptionalField.V2
import qualified ExampleApiChanges.ModifyOptionalToListField.V1
import qualified ExampleApiChanges.ModifyOptionalToListField.V2
import qualified ExampleApiChanges.RemoveConstructor.V1
import qualified ExampleApiChanges.RemoveConstructor.V2
import qualified ExampleApis.Api
import qualified ExampleApis.InvalidService.DuplicateConstructorName
import qualified ExampleApis.InvalidService.DuplicateEndpointName
import qualified ExampleApis.InvalidService.DuplicateTypeName
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
import qualified Interop.Compatibility
import qualified Interop.Spec
import qualified Interop.Wire
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
      checkParallel (Group "diff" (diffTest <$> changeApiExamples)),
      checkParallel (Group "compile error" (compileErrorTest <$> compileErrorExamples)),
      checkParallel (Group "backwards-compatible decoding" (backwardsCompatibleDecodingTest <$> changeTypeExamples)),
      checkParallel (Group "ruby client generation" rubyClientGenerationTests),
      checkParallel (Group "generated ruby code" generatedRubyCodeTests),
      checkParallel (Group "create service error" (createServiceErrorTest <$> exampleInvalidServices))
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

diffTest :: ChangeApiExample -> (PropertyName, Property)
diffTest (ChangeApiExample name v1Api v2Api) =
  let path = "test/ExampleApiChanges/" <> name <> "/V2.hs"
   in test1 (fromString path) $
        do
          let warnings =
                Interop.Compatibility.check
                  (Interop.Spec.spec v1Api)
                  (Interop.Spec.spec v2Api)
          either id (\_ -> "No warnings.") warnings
          & equalToCommentsInFile "Warnings for this change from Base type:" path

backwardsCompatibleDecodingTest :: ChangeTypeExample -> (PropertyName, Property)
backwardsCompatibleDecodingTest (ChangeTypeExample name gen (_ :: proxy type_)) =
  test (fromString ("Can still decode type after change: " <> name)) $ do
    oldType <- forAll gen
    let encoded = encode oldType
    (_ :: type_) <- evalEither (decode encoded)
    pure ()

rubyClientGenerationTests :: [(PropertyName, Property)]
rubyClientGenerationTests =
  [ test1 "ExampleApi" $ do
      (path, h) <- evalIO $ System.IO.openTempFile "/tmp" "interop-tests-ruby-generation.rb"
      evalIO $ System.IO.hClose h
      evalIO $ Interop.generateRubyClient path ["Apis", "ExampleApi"] ExampleApis.Api.service
      generated <- evalIO $ Data.Text.IO.readFile path
      equalToContentsOfFile "test/ruby-tests/apis/example_api.rb" generated
  ]
    <> ( changeApiExamples
           & fmap
             ( \(ChangeApiExample name _ v2Api) ->
                 test1 (fromString name) $ do
                   (path, h) <- evalIO $ System.IO.openTempFile "/tmp" "interop-tests-ruby-generation.rb"
                   evalIO $ System.IO.hClose h
                   evalIO $ Interop.generateRubyClient path ["Apis", Text.pack name, "V2"] v2Api
                   generated <- evalIO $ Data.Text.IO.readFile path
                   equalToContentsOfFile ("test/ExampleApiChanges/" <> name <> "/v2.rb") generated
             )
       )

generatedRubyCodeTests :: [(PropertyName, Property)]
generatedRubyCodeTests =
  [ test1 "Generated ruby code" $ do
      let app =
            ExampleApis.Api.endpoints
              ++ ExampleApiChanges.AddConstructor.V1.endpoints
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

createServiceErrorTest :: (FilePath, [Interop.Endpoint m]) -> (PropertyName, Property)
createServiceErrorTest (path, endpoints) =
  test1 (fromString path) $
    case Interop.service endpoints of
      Right _ -> fail "Expected service creation to fail, but it did not."
      Left err -> equalToCommentsInFile "Interop.service fails with:" path err

exampleInvalidServices :: [(FilePath, [Interop.Endpoint IO])]
exampleInvalidServices =
  [ ( "test/ExampleApis/InvalidService/DuplicateEndpointName.hs",
      ExampleApis.InvalidService.DuplicateEndpointName.endpoints
    ),
    ( "test/ExampleApis/InvalidService/DuplicateTypeName.hs",
      ExampleApis.InvalidService.DuplicateTypeName.endpoints
    ),
    ( "test/ExampleApis/InvalidService/DuplicateConstructorName.hs",
      ExampleApis.InvalidService.DuplicateConstructorName.endpoints
    )
  ]

data ExampleType where
  ExampleType ::
    (Interop.Wire a, Show a, Eq a) =>
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

data ChangeApiExample where
  ChangeApiExample ::
    FilePath ->
    Interop.Service a ->
    Interop.Service b ->
    ChangeApiExample

changeApiExamples :: [ChangeApiExample]
changeApiExamples =
  [ ChangeApiExample
      "AddConstructor"
      ExampleApiChanges.AddConstructor.V1.service
      ExampleApiChanges.AddConstructor.V2.service,
    ChangeApiExample
      "AddEndpoint"
      ExampleApiChanges.AddEndpoint.V1.service
      ExampleApiChanges.AddEndpoint.V2.service,
    ChangeApiExample
      "AddFirstField"
      ExampleApiChanges.AddFirstField.V1.service
      ExampleApiChanges.AddFirstField.V2.service,
    ChangeApiExample
      "AddNonOptionalField"
      ExampleApiChanges.AddNonOptionalField.V1.service
      ExampleApiChanges.AddNonOptionalField.V2.service,
    ChangeApiExample
      "AddOptionalField"
      ExampleApiChanges.AddOptionalField.V1.service
      ExampleApiChanges.AddOptionalField.V2.service,
    ChangeApiExample
      "AddListField"
      ExampleApiChanges.AddListField.V1.service
      ExampleApiChanges.AddListField.V2.service,
    ChangeApiExample
      "AddDictField"
      ExampleApiChanges.AddDictField.V1.service
      ExampleApiChanges.AddDictField.V2.service,
    ChangeApiExample
      "DropNonOptionalField"
      ExampleApiChanges.DropNonOptionalField.V1.service
      ExampleApiChanges.DropNonOptionalField.V2.service,
    ChangeApiExample
      "DropOptionalField"
      ExampleApiChanges.DropOptionalField.V1.service
      ExampleApiChanges.DropOptionalField.V2.service,
    ChangeApiExample
      "DropListField"
      ExampleApiChanges.DropListField.V1.service
      ExampleApiChanges.DropListField.V2.service,
    ChangeApiExample
      "DropAllFields"
      ExampleApiChanges.DropAllFields.V1.service
      ExampleApiChanges.DropAllFields.V2.service,
    ChangeApiExample
      "ModifyListToOptionalField"
      ExampleApiChanges.ModifyListToOptionalField.V1.service
      ExampleApiChanges.ModifyListToOptionalField.V2.service,
    ChangeApiExample
      "ModifyOptionalToListField"
      ExampleApiChanges.ModifyOptionalToListField.V1.service
      ExampleApiChanges.ModifyOptionalToListField.V2.service,
    ChangeApiExample
      "ModifyFieldType"
      ExampleApiChanges.ModifyFieldType.V1.service
      ExampleApiChanges.ModifyFieldType.V2.service,
    ChangeApiExample
      "RemoveConstructor"
      ExampleApiChanges.RemoveConstructor.V1.service
      ExampleApiChanges.RemoveConstructor.V2.service
  ]

data ChangeTypeExample where
  ChangeTypeExample ::
    (Show v1, Interop.Wire v1, Interop.Wire v2) =>
    FilePath ->
    Hedgehog.Gen v1 ->
    proxy v2 ->
    ChangeTypeExample

changeTypeExamples :: [ChangeTypeExample]
changeTypeExamples =
  [ ChangeTypeExample
      "AddFirstField"
      ExampleApiChanges.AddFirstField.V1.gen
      (Proxy :: Proxy ExampleApiChanges.AddFirstField.V2.TestType),
    ChangeTypeExample
      "AddOptionalField"
      ExampleApiChanges.AddOptionalField.V1.gen
      (Proxy :: Proxy ExampleApiChanges.AddOptionalField.V2.TestType),
    ChangeTypeExample
      "AddListField"
      ExampleApiChanges.AddListField.V1.gen
      (Proxy :: Proxy ExampleApiChanges.AddListField.V2.TestType),
    ChangeTypeExample
      "AddDictField"
      ExampleApiChanges.AddDictField.V1.gen
      (Proxy :: Proxy ExampleApiChanges.AddDictField.V2.TestType),
    ChangeTypeExample
      "DropNonOptionalField"
      ExampleApiChanges.DropNonOptionalField.V1.gen
      (Proxy :: Proxy ExampleApiChanges.DropNonOptionalField.V2.TestType),
    ChangeTypeExample
      "DropOptionalField"
      ExampleApiChanges.DropOptionalField.V1.gen
      (Proxy :: Proxy ExampleApiChanges.DropOptionalField.V2.TestType),
    ChangeTypeExample
      "DropListField"
      ExampleApiChanges.DropListField.V1.gen
      (Proxy :: Proxy ExampleApiChanges.DropListField.V2.TestType),
    ChangeTypeExample
      "DropAllFields"
      ExampleApiChanges.DropAllFields.V1.gen
      (Proxy :: Proxy ExampleApiChanges.DropAllFields.V2.TestType)
  ]

getCompileErrorExamples :: IO [FilePath]
getCompileErrorExamples =
  let dir = "test/example-compile-errors/"
   in fmap (dir <>) <$> Directory.listDirectory dir

encode :: Interop.Wire a => a -> ByteString
encode = Encoding.encodingToLazyByteString . Interop.Wire.encode

decode :: Interop.Wire a => ByteString -> Either String a
decode = Aeson.parseEither Interop.Wire.decode <=< Aeson.eitherDecode

test :: PropertyName -> PropertyT IO () -> (PropertyName, Property)
test description prop = (description, property prop)

test1 :: PropertyName -> PropertyT IO () -> (PropertyName, Property)
test1 description prop = (description, withTests 1 (property prop))

encodePretty :: Interop.Wire a => a -> ByteString
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
