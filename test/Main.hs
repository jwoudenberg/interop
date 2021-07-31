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
import qualified ExampleApis.AddConstructor.V1
import qualified ExampleApis.AddConstructor.V2
import qualified ExampleApis.AddDictField.V1
import qualified ExampleApis.AddDictField.V2
import qualified ExampleApis.AddEndpoint.V1
import qualified ExampleApis.AddEndpoint.V2
import qualified ExampleApis.AddFirstField.V1
import qualified ExampleApis.AddFirstField.V2
import qualified ExampleApis.AddFirstFieldToSecondConstructor.V1
import qualified ExampleApis.AddFirstFieldToSecondConstructor.V2
import qualified ExampleApis.AddListField.V1
import qualified ExampleApis.AddListField.V2
import qualified ExampleApis.AddNonOptionalField.V1
import qualified ExampleApis.AddNonOptionalField.V2
import qualified ExampleApis.AddOptionalField.V1
import qualified ExampleApis.AddOptionalField.V2
import qualified ExampleApis.AddSeqField.V1
import qualified ExampleApis.AddSeqField.V2
import qualified ExampleApis.AddSetField.V1
import qualified ExampleApis.AddSetField.V2
import qualified ExampleApis.DropAllFields.V1
import qualified ExampleApis.DropAllFields.V2
import qualified ExampleApis.DropListField.V1
import qualified ExampleApis.DropListField.V2
import qualified ExampleApis.DropNonOptionalField.V1
import qualified ExampleApis.DropNonOptionalField.V2
import qualified ExampleApis.DropOptionalField.V1
import qualified ExampleApis.DropOptionalField.V2
import qualified ExampleApis.EchoTypes.Api
import qualified ExampleApis.InvalidService.DuplicateConstructorName
import qualified ExampleApis.InvalidService.DuplicateEndpointName
import qualified ExampleApis.InvalidService.DuplicateTypeName
import qualified ExampleApis.ModifyFieldType.V1
import qualified ExampleApis.ModifyFieldType.V2
import qualified ExampleApis.ModifyListToOptionalField.V1
import qualified ExampleApis.ModifyListToOptionalField.V2
import qualified ExampleApis.ModifyOptionalToListField.V1
import qualified ExampleApis.ModifyOptionalToListField.V2
import qualified ExampleApis.RemoveConstructor.V1
import qualified ExampleApis.RemoveConstructor.V2
import qualified ExampleTypes.Bool
import qualified ExampleTypes.Dict
import qualified ExampleTypes.Double
import qualified ExampleTypes.EnumType
import qualified ExampleTypes.Float
import qualified ExampleTypes.Int
import qualified ExampleTypes.Int16
import qualified ExampleTypes.Int32
import qualified ExampleTypes.Int64
import qualified ExampleTypes.Int8
import qualified ExampleTypes.List
import qualified ExampleTypes.Maybe
import qualified ExampleTypes.NestedRecord
import qualified ExampleTypes.NestedType
import qualified ExampleTypes.Record
import qualified ExampleTypes.RecursiveType
import qualified ExampleTypes.Seq
import qualified ExampleTypes.Set
import qualified ExampleTypes.Text
import qualified ExampleTypes.Unit
import qualified ExampleTypes.Word
import qualified ExampleTypes.Word16
import qualified ExampleTypes.Word32
import qualified ExampleTypes.Word64
import qualified ExampleTypes.Word8
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
diffTest (ChangeApiExample {apiPath, v1Api, v2Api}) =
  let path = "test/ExampleApis/" <> apiPath <> "/V2.hs"
   in test1 (fromString path) $
        do
          let warnings =
                Interop.Compatibility.check
                  (Interop.Spec.spec v1Api)
                  (Interop.Spec.spec v2Api)
          either id (\_ -> "No warnings.") warnings
          & equalToCommentsInFile "Warnings for this change from Base type:" path

backwardsCompatibleDecodingTest :: ChangeTypeExample -> (PropertyName, Property)
backwardsCompatibleDecodingTest (ChangeTypeExample apiPath gen (_ :: proxy type_)) =
  test (fromString ("Can still decode type after change: " <> apiPath)) $ do
    oldType <- forAll gen
    let encoded = encode oldType
    (_ :: type_) <- evalEither (decode encoded)
    pure ()

rubyClientGenerationTests :: [(PropertyName, Property)]
rubyClientGenerationTests =
  [ test1 "ExampleApi" $ do
      (path, h) <- evalIO $ System.IO.openTempFile "/tmp" "interop-tests-ruby-generation.rb"
      evalIO $ System.IO.hClose h
      evalIO $ Interop.generateRubyClient path ["Apis", "EchoTypes", "Api"] ExampleApis.EchoTypes.Api.service
      generated <- evalIO $ Data.Text.IO.readFile path
      equalToContentsOfFile "test/ExampleApis/EchoTypes/api.rb" generated
  ]
    <> ( changeApiExamples
           & fmap
             ( \(ChangeApiExample {apiPath, v2Api}) ->
                 test1 (fromString apiPath) $ do
                   (path, h) <- evalIO $ System.IO.openTempFile "/tmp" "interop-tests-ruby-generation.rb"
                   evalIO $ System.IO.hClose h
                   evalIO $ Interop.generateRubyClient path ["Apis", Text.pack apiPath, "V2"] v2Api
                   generated <- evalIO $ Data.Text.IO.readFile path
                   equalToContentsOfFile ("test/ExampleApis/" <> apiPath <> "/v2.rb") generated
             )
       )

generatedRubyCodeTests :: [(PropertyName, Property)]
generatedRubyCodeTests =
  [ test1 "Generated ruby code" $ do
      let app =
            ExampleApis.EchoTypes.Api.endpoints
              ++ concatMap (\ChangeApiExample {rubyTestEndpoints} -> rubyTestEndpoints) changeApiExamples
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
  [ ExampleType "test/ExampleTypes/Bool.hs" (ExampleTypes.Bool.example) (ExampleTypes.Bool.gen),
    ExampleType "test/ExampleTypes/Dict.hs" (ExampleTypes.Dict.example) (ExampleTypes.Dict.gen),
    ExampleType "test/ExampleTypes/Double.hs" (ExampleTypes.Double.example) (ExampleTypes.Double.gen),
    ExampleType "test/ExampleTypes/EnumType.hs" (ExampleTypes.EnumType.example) (ExampleTypes.EnumType.gen),
    ExampleType "test/ExampleTypes/Float.hs" (ExampleTypes.Float.example) (ExampleTypes.Float.gen),
    ExampleType "test/ExampleTypes/Int.hs" (ExampleTypes.Int.example) (ExampleTypes.Int.gen),
    ExampleType "test/ExampleTypes/Int8.hs" (ExampleTypes.Int8.example) (ExampleTypes.Int8.gen),
    ExampleType "test/ExampleTypes/Int16.hs" (ExampleTypes.Int16.example) (ExampleTypes.Int16.gen),
    ExampleType "test/ExampleTypes/Int32.hs" (ExampleTypes.Int32.example) (ExampleTypes.Int32.gen),
    ExampleType "test/ExampleTypes/Int64.hs" (ExampleTypes.Int64.example) (ExampleTypes.Int64.gen),
    ExampleType "test/ExampleTypes/List.hs" (ExampleTypes.List.example) (ExampleTypes.List.gen),
    ExampleType "test/ExampleTypes/Maybe.hs" (ExampleTypes.Maybe.example) (ExampleTypes.Maybe.gen),
    ExampleType "test/ExampleTypes/NestedType.hs" (ExampleTypes.NestedType.example) (ExampleTypes.NestedType.gen),
    ExampleType "test/ExampleTypes/NestedRecord.hs" (ExampleTypes.NestedRecord.example) (ExampleTypes.NestedRecord.gen),
    ExampleType "test/ExampleTypes/Record.hs" (ExampleTypes.Record.example) (ExampleTypes.Record.gen),
    ExampleType "test/ExampleTypes/RecursiveType.hs" (ExampleTypes.RecursiveType.example) (ExampleTypes.RecursiveType.gen),
    ExampleType "test/ExampleTypes/Set.hs" (ExampleTypes.Set.example) (ExampleTypes.Set.gen),
    ExampleType "test/ExampleTypes/Seq.hs" (ExampleTypes.Seq.example) (ExampleTypes.Seq.gen),
    ExampleType "test/ExampleTypes/Text.hs" (ExampleTypes.Text.example) (ExampleTypes.Text.gen),
    ExampleType "test/ExampleTypes/Unit.hs" (ExampleTypes.Unit.example) (ExampleTypes.Unit.gen),
    ExampleType "test/ExampleTypes/Word.hs" (ExampleTypes.Word.example) (ExampleTypes.Word.gen),
    ExampleType "test/ExampleTypes/Word8.hs" (ExampleTypes.Word8.example) (ExampleTypes.Word8.gen),
    ExampleType "test/ExampleTypes/Word16.hs" (ExampleTypes.Word16.example) (ExampleTypes.Word16.gen),
    ExampleType "test/ExampleTypes/Word32.hs" (ExampleTypes.Word32.example) (ExampleTypes.Word32.gen),
    ExampleType "test/ExampleTypes/Word64.hs" (ExampleTypes.Word64.example) (ExampleTypes.Word64.gen)
  ]

data ChangeApiExample where
  ChangeApiExample ::
    { apiPath :: FilePath,
      rubyTestEndpoints :: [Interop.Endpoint IO],
      v1Api :: Interop.Service IO,
      v2Api :: Interop.Service b
    } ->
    ChangeApiExample

changeApiExamples :: [ChangeApiExample]
changeApiExamples =
  [ ChangeApiExample
      "AddConstructor"
      ExampleApis.AddConstructor.V1.endpoints
      ExampleApis.AddConstructor.V1.service
      ExampleApis.AddConstructor.V2.service,
    ChangeApiExample
      "AddEndpoint"
      ExampleApis.AddEndpoint.V1.endpoints
      ExampleApis.AddEndpoint.V1.service
      ExampleApis.AddEndpoint.V2.service,
    ChangeApiExample
      "AddFirstField"
      ExampleApis.AddFirstField.V1.endpoints
      ExampleApis.AddFirstField.V1.service
      ExampleApis.AddFirstField.V2.service,
    ChangeApiExample
      "AddFirstFieldToSecondConstructor"
      ExampleApis.AddFirstFieldToSecondConstructor.V1.endpoints
      ExampleApis.AddFirstFieldToSecondConstructor.V1.service
      ExampleApis.AddFirstFieldToSecondConstructor.V2.service,
    ChangeApiExample
      "AddNonOptionalField"
      ExampleApis.AddNonOptionalField.V1.endpoints
      ExampleApis.AddNonOptionalField.V1.service
      ExampleApis.AddNonOptionalField.V2.service,
    ChangeApiExample
      "AddOptionalField"
      ExampleApis.AddOptionalField.V1.endpoints
      ExampleApis.AddOptionalField.V1.service
      ExampleApis.AddOptionalField.V2.service,
    ChangeApiExample
      "AddListField"
      ExampleApis.AddListField.V1.endpoints
      ExampleApis.AddListField.V1.service
      ExampleApis.AddListField.V2.service,
    ChangeApiExample
      "AddDictField"
      ExampleApis.AddDictField.V1.endpoints
      ExampleApis.AddDictField.V1.service
      ExampleApis.AddDictField.V2.service,
    ChangeApiExample
      "AddSetField"
      ExampleApis.AddSetField.V1.endpoints
      ExampleApis.AddSetField.V1.service
      ExampleApis.AddSetField.V2.service,
    ChangeApiExample
      "AddSeqField"
      ExampleApis.AddSeqField.V1.endpoints
      ExampleApis.AddSeqField.V1.service
      ExampleApis.AddSeqField.V2.service,
    ChangeApiExample
      "DropNonOptionalField"
      ExampleApis.DropNonOptionalField.V1.endpoints
      ExampleApis.DropNonOptionalField.V1.service
      ExampleApis.DropNonOptionalField.V2.service,
    ChangeApiExample
      "DropOptionalField"
      ExampleApis.DropOptionalField.V1.endpoints
      ExampleApis.DropOptionalField.V1.service
      ExampleApis.DropOptionalField.V2.service,
    ChangeApiExample
      "DropListField"
      ExampleApis.DropListField.V1.endpoints
      ExampleApis.DropListField.V1.service
      ExampleApis.DropListField.V2.service,
    ChangeApiExample
      "DropAllFields"
      ExampleApis.DropAllFields.V1.endpoints
      ExampleApis.DropAllFields.V1.service
      ExampleApis.DropAllFields.V2.service,
    ChangeApiExample
      "ModifyListToOptionalField"
      ExampleApis.ModifyListToOptionalField.V1.endpoints
      ExampleApis.ModifyListToOptionalField.V1.service
      ExampleApis.ModifyListToOptionalField.V2.service,
    ChangeApiExample
      "ModifyOptionalToListField"
      ExampleApis.ModifyOptionalToListField.V1.endpoints
      ExampleApis.ModifyOptionalToListField.V1.service
      ExampleApis.ModifyOptionalToListField.V2.service,
    ChangeApiExample
      "ModifyFieldType"
      ExampleApis.ModifyFieldType.V1.endpoints
      ExampleApis.ModifyFieldType.V1.service
      ExampleApis.ModifyFieldType.V2.service,
    ChangeApiExample
      "RemoveConstructor"
      ExampleApis.RemoveConstructor.V1.endpoints
      ExampleApis.RemoveConstructor.V1.service
      ExampleApis.RemoveConstructor.V2.service
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
      "AddFirstFieldToSecondConstructor"
      ExampleApis.AddFirstFieldToSecondConstructor.V1.gen
      (Proxy :: Proxy ExampleApis.AddFirstFieldToSecondConstructor.V2.AddFirstFieldToSecondConstructorType),
    ChangeTypeExample
      "AddFirstField"
      ExampleApis.AddFirstField.V1.gen
      (Proxy :: Proxy ExampleApis.AddFirstField.V2.AddFirstFieldType),
    ChangeTypeExample
      "AddOptionalField"
      ExampleApis.AddOptionalField.V1.gen
      (Proxy :: Proxy ExampleApis.AddOptionalField.V2.AddOptionalFieldType),
    ChangeTypeExample
      "AddListField"
      ExampleApis.AddListField.V1.gen
      (Proxy :: Proxy ExampleApis.AddListField.V2.AddListFieldType),
    ChangeTypeExample
      "AddDictField"
      ExampleApis.AddDictField.V1.gen
      (Proxy :: Proxy ExampleApis.AddDictField.V2.AddDictFieldType),
    ChangeTypeExample
      "AddSetField"
      ExampleApis.AddSetField.V1.gen
      (Proxy :: Proxy ExampleApis.AddSetField.V2.AddSetFieldType),
    ChangeTypeExample
      "AddSeqField"
      ExampleApis.AddSeqField.V1.gen
      (Proxy :: Proxy ExampleApis.AddSeqField.V2.AddSeqFieldType),
    ChangeTypeExample
      "DropNonOptionalField"
      ExampleApis.DropNonOptionalField.V1.gen
      (Proxy :: Proxy ExampleApis.DropNonOptionalField.V2.DropNonOptionalFieldType),
    ChangeTypeExample
      "DropOptionalField"
      ExampleApis.DropOptionalField.V1.gen
      (Proxy :: Proxy ExampleApis.DropOptionalField.V2.DropOptionalFieldType),
    ChangeTypeExample
      "DropListField"
      ExampleApis.DropListField.V1.gen
      (Proxy :: Proxy ExampleApis.DropListField.V2.DropListFieldType),
    ChangeTypeExample
      "DropAllFields"
      ExampleApis.DropAllFields.V1.gen
      (Proxy :: Proxy ExampleApis.DropAllFields.V2.DropAllFieldsType)
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
