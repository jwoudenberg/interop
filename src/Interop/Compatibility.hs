{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Find differences between types and explain whether they're backwards
-- compatible or not.
module Interop.Compatibility
  ( checkServerClientCompatibility,
    check,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Function ((&))
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Interop.Spec as Spec
import Interop.Wire.Flat

checkServerClientCompatibility :: FilePath -> FilePath -> IO (Either Text ())
checkServerClientCompatibility serverPath clientPath = do
  serverResult <- readSpec serverPath
  clientResult <- readSpec clientPath
  pure $ do
    server <- serverResult
    client <- clientResult
    check server client

readSpec :: FilePath -> IO (Either Text Spec.Service)
readSpec path = do
  contents <- ByteString.readFile path
  let lines' = ByteString.split 10 {- \n -} contents
  let maybeSpec = do
        json <- findSpecInLine (ByteString.tails (last lines'))
        Aeson.decode json
  case maybeSpec of
    Nothing -> pure (Left ("Not an interop spec file: " <> Text.pack path))
    Just spec -> pure (Right spec)

findSpecInLine :: [ByteString.ByteString] -> Maybe ByteString.ByteString
findSpecInLine [] = Nothing
findSpecInLine (first : rest) =
  case ByteString.stripPrefix "INTEROP-SPEC:" first of
    Nothing -> findSpecInLine rest
    Just json -> Just json

check :: Spec.Service -> Spec.Service -> Either Text ()
check server client =
  merge
    (\_ _ acc -> acc) -- server-only endpoints are fine
    ( \endpointName serverEndpoint clientEndpoint acc ->
        let requestTypeWarnings =
              diffType
                (InEndpointRequest endpointName)
                (Spec.customTypes server, Spec.requestType serverEndpoint)
                (Spec.customTypes client, Spec.requestType clientEndpoint)
            responseTypeWarnings =
              diffType
                (InEndpointResponse endpointName)
                (Spec.customTypes server, Spec.responseType serverEndpoint)
                (Spec.customTypes client, Spec.responseType clientEndpoint)
         in requestTypeWarnings <> responseTypeWarnings <> acc
    )
    ( \endpointName _ acc ->
        clientEndpointDoesntExist (InEndpoint endpointName) : acc
    )
    (Map.toList (Spec.endpoints server))
    (Map.toList (Spec.endpoints client))
    []
    & ( \case
          [] -> Right ()
          first : rest ->
            foldl' (\acc entry -> acc <> "\n\n" <> warningToText entry) (warningToText first) rest
              & Builder.toLazyText
              & Data.Text.Lazy.toStrict
              & Left
      )

data Path (context :: PathContext) where
  InEndpoint :: Text -> Path 'Endpoint
  InEndpointRequest :: Text -> Path 'Request
  InEndpointResponse :: Text -> Path 'Response
  InType :: Text -> Path context -> Path context
  InConstructor :: Text -> Path context -> Path context
  InField :: Text -> Path context -> Path context

data PathContext
  = Request
  | Response
  | Endpoint

data Warning where
  Warning ::
    { short :: Builder.Builder,
      detailed :: Builder.Builder,
      context :: Path context
    } ->
    Warning

ifUsedInRequest :: (Path 'Request -> a) -> Path context -> [a]
ifUsedInRequest fn path = go path
  where
    go path' =
      case path' of
        InEndpoint _ -> []
        InEndpointRequest _ -> [fn path]
        InEndpointResponse _ -> []
        InType _ subPath -> go subPath
        InConstructor _ subPath -> go subPath
        InField _ subPath -> go subPath

ifUsedInResponse :: (Path 'Response -> a) -> Path context -> [a]
ifUsedInResponse fn path = go path
  where
    go path' =
      case path' of
        InEndpoint _ -> []
        InEndpointRequest _ -> []
        InEndpointResponse _ -> [fn path]
        InType _ subPath -> go subPath
        InConstructor _ subPath -> go subPath
        InField _ subPath -> go subPath

warningToText :: Warning -> Builder.Builder
warningToText Warning {short, detailed, context} =
  short <> "\n" <> renderContext context <> "\n\n" <> detailed

renderContext :: Path context -> Builder.Builder
renderContext (InField fieldName rest) = renderContext rest <> " { " <> Builder.fromText fieldName <> " }"
renderContext (InConstructor constructorName rest) = renderContext rest <> " = " <> Builder.fromText constructorName
renderContext (InType typeName _) = "data " <> Builder.fromText typeName
renderContext (InEndpoint _) = ""
renderContext (InEndpointRequest _) = ""
renderContext (InEndpointResponse _) = ""

serverAlwaysExpectsTypeButClientThinksItOptional :: Path 'Request -> Warning
serverAlwaysExpectsTypeButClientThinksItOptional path =
  let type_ = Builder.fromText (pathType path)
   in Warning
        { short = "The server always expects types '" <> type_ <> "' on requests, but the generated client code considers it optional.",
          context = path,
          detailed =
            "Maybe you're trying to make a type non-optional? If so, the following steps allow you to do so safely:\n"
              <> "\n"
              <> "1. Change the client to always send values for the optional type.\n"
              <> "2. Make sure the changes from step 1 are deployed.\n"
              <> "3. Make the type non-optional in the server code.\n"
              <> "\n"
              <> "If you're currently at step 3 then this warning is expected."
        }

serverMightNotReturnValueButClientExpectsIt :: Path 'Response -> Warning
serverMightNotReturnValueButClientExpectsIt path =
  let type_ = Builder.fromText (pathType path)
   in Warning
        { short = "Generated client code always expects types '" <> type_ <> "' on responses, but server considers it optional.",
          context = path,
          detailed =
            "Maybe you're trying to make a type optional? If so, the following steps allow you to do so safely:\n"
              <> "\n"
              <> "1. Make the type optional by wrapping it in a 'Maybe', but don't return 'Nothing' values from the server yet.\n"
              <> "2. Change the client to make it support responses that omit the type.\n"
              <> "3. Make sure the changes from step 1 and 2 are deployed.\n"
              <> "4. You can now start returning 'Nothing' values from the server!\n"
              <> "\n"
              <> "If you're currently at step 1 then this warning is expected."
        }

incompatibleRequestTypes :: Path 'Request -> Warning
incompatibleRequestTypes path =
  let endpoint = Builder.fromText (pathEndpoint path)
      type_ = Builder.fromText (pathType path)
   in Warning
        { short = "The server expects an entirely different type '" <> type_ <> "' from the '" <> endpoint <> "' endpoint than the one the generated client code sends.",
          context = path,
          detailed =
            "Maybe you're trying to change the type accepted by and endpoint? If so, the following steps allow you to do so safely:\n"
              <> "\n"
              <> "1. Create an entirely new endpoint that's like the old one, except it accepts your new type.\n"
              <> "2. Change the client to only use the new endpoint.\n"
              <> "3. Make sure changes from step 1 and 2 are deployed.\n"
              <> "4. Delete the old endpoint."
        }

incompatibleResponseTypes :: Path 'Response -> Warning
incompatibleResponseTypes path =
  let endpoint = Builder.fromText (pathEndpoint path)
      type_ = Builder.fromText (pathType path)
   in Warning
        { short = "The server returns an entirely different type '" <> type_ <> "' from the '" <> endpoint <> "' endpoint than the one the generated client code expects.",
          context = path,
          detailed =
            "Maybe you're trying to change the type returned by and endpoint? If so, the following steps allow you to do so safely:\n"
              <> "\n"
              <> "1. Create an entirely new endpoint that's like the old one, except it returns your new type.\n"
              <> "2. Change the client to only use the new endpoint.\n"
              <> "3. Make sure changes from step 1 and 2 are deployed.\n"
              <> "4. Delete the old endpoint."
        }

serverWontAcceptConstructor :: Path 'Request -> Warning
serverWontAcceptConstructor path =
  let type_ = Builder.fromText (pathType path)
      constructor = Builder.fromText (pathConstructor path)
   in Warning
        { short = "The generated client code supports use the '" <> constructor <> "' constructor of the type '" <> type_ <> "' in requests, but the server doesn't know this constructor",
          context = path,
          detailed =
            "Maybe you're trying to remove a constructor from a type? If so, make sure to follow these steps:\n"
              <> "\n"
              <> "1. Stop using the constructor in server code.\n"
              <> "2. Make sure the changes from step 1 are deployed.\n"
              <> "3. Remove the constructor from the server code.\n"
              <> "\n"
              <> "If you're currently at step 1 then this warning is expected."
        }

clientWontAcceptConstructor :: Path 'Response -> Warning
clientWontAcceptConstructor path =
  let type_ = Builder.fromText (pathType path)
      constructor = Builder.fromText (pathConstructor path)
   in Warning
        { short = "The server might return the '" <> constructor <> "' constructor of the type '" <> type_ <> "', but the generated client code doesn't know that constructor.",
          context = path,
          detailed =
            "Maybe you're trying to add a new constructor to a type? If so, make sure to follow these steps:\n"
              <> "\n"
              <> "1. Add the constructor to the type but make sure not to use it in server responses yet.\n"
              <> "2. Change the client to support the new constructor.\n"
              <> "3. Make sure the changes from step 1 and 2 are deployed.\n"
              <> "4. Now you can start using the constructor in server code!\n"
              <> "\n"
              <> "If you're currently at step 1 then this warning is expected."
        }

serverWontReturnExpectedField :: Path 'Response -> Warning
serverWontReturnExpectedField path =
  let type_ = Builder.fromText (pathType path)
      field = Builder.fromText (pathField path)
   in Warning
        { short = "The generated client code expects record '" <> type_ <> "' to have a field '" <> field <> "', but server responses don't include such a field.",
          context = path,
          detailed =
            "Maybe you're trying to remove a field from a record? If so, make sure to follow these steps:\n"
              <> "\n"
              <> "1. Make the field optional by wrapping it in a 'Maybe', but don't return 'Nothing' values from the server yet.\n"
              <> "2. Change the client to make it support responses that omit the field.\n"
              <> "3. Make sure the changes from step 1 and 2 are deployed.\n"
              <> "4. Remove the field from the server code.\n"
              <> "\n"
              <> "It looks like you missed step 1, because '"
              <> field
              <> "' isn't an optional field."
        }

clientDoesntSendMandatoryField :: Path 'Request -> Warning
clientDoesntSendMandatoryField path =
  let type_ = Builder.fromText (pathType path)
      field = Builder.fromText (pathField path)
   in Warning
        { short = "The server expects record '" <> type_ <> "' to have a field '" <> field <> "', but client requests don't include such a field.",
          context = path,
          detailed =
            "Maybe you're trying to add a new field to a record? If so, make sure to follow these steps:\n"
              <> "\n"
              <> "1. Add the field to the server, but wrap it in a 'Maybe' to keep it optional for now.\n"
              <> "2. Change the client to always send the new field.\n"
              <> "3. Make sure the changes from step 1 and 2 are deployed.\n"
              <> "4. Make the field mandatory in the server by removing the 'Maybe'.\n"
              <> "\n"
              <> "It looks like you missed step 1, because '"
              <> field
              <> "' isn't an optional field."
        }

clientEndpointDoesntExist :: Path 'Endpoint -> Warning
clientEndpointDoesntExist path =
  let endpoint = Builder.fromText (pathEndpoint path)
   in Warning
        { short = "The generated client code supports an endpoint '" <> endpoint <> "', but the server doesn't have such an endpoint.",
          context = path,
          detailed =
            "Maybe you're trying to remove an endpoint? If so, make sure to follow these steps:\n"
              <> "\n"
              <> "1. Stop using the endpoint in your client code.\n"
              <> "2. Make sure the clients from step 1 are deployed.\n"
              <> "3. Remove the endpoint from the server code.\n"
              <> "\n"
              <> "If you're currently at step 3 this warning is expected."
        }

pathEndpoint :: Path a -> Text
pathEndpoint path =
  case path of
    InEndpoint name -> name
    InEndpointRequest name -> name
    InEndpointResponse name -> name
    InType _ subPath -> pathEndpoint subPath
    InConstructor _ subPath -> pathEndpoint subPath
    InField _ subPath -> pathEndpoint subPath

pathType :: Path a -> Text
pathType path =
  case path of
    InEndpoint _ -> ""
    InEndpointRequest _ -> ""
    InEndpointResponse _ -> ""
    InType name _ -> name
    InConstructor _ subPath -> pathType subPath
    InField _ subPath -> pathType subPath

pathConstructor :: Path a -> Text
pathConstructor path =
  case path of
    InEndpoint _ -> ""
    InEndpointRequest _ -> ""
    InEndpointResponse _ -> ""
    InType _ _ -> ""
    InConstructor name _ -> name
    InField _ subPath -> pathType subPath

pathField :: Path a -> Text
pathField path =
  case path of
    InEndpoint _ -> ""
    InEndpointRequest _ -> ""
    InEndpointResponse _ -> ""
    InType _ _ -> ""
    InConstructor _ _ -> ""
    InField name _ -> name

diffType ::
  Path context ->
  (Map.Map Text CustomType, Type) ->
  (Map.Map Text CustomType, Type) ->
  [Warning]
diffType path (serverTypes, server) (clientTypes, client) =
  case (server, client) of
    (NestedCustomType serverName, NestedCustomType clientName) -> do
      serverType <- maybe [] pure (Map.lookup serverName serverTypes)
      clientType <- maybe [] pure (Map.lookup clientName clientTypes)
      case (subTypes serverType, subTypes clientType) of
        (Right serverConstructors, Right clientConstructors) ->
          diffCustomType
            (InType (typeAsText server) path)
            (serverTypes, serverConstructors)
            (clientTypes, clientConstructors)
        (Left serverFields, Left clientFields) ->
          diffFields
            (InType (typeAsText server) path)
            (serverTypes, serverFields)
            (clientTypes, clientFields)
        (_, _) ->
          ifUsedInResponse incompatibleResponseTypes (InType (typeAsText server) path)
    (List subServer, List subClient) ->
      diffType (InType (typeAsText server) path) (serverTypes, subServer) (clientTypes, subClient)
    (Dict keyServer valServer, Dict keyClient valClient) ->
      diffType (InType (typeAsText server) path) (serverTypes, keyServer) (clientTypes, keyClient)
        <> diffType (InType (typeAsText server) path) (serverTypes, valServer) (clientTypes, valClient)
    (Optional subServer, Optional subClient) ->
      diffType (InType (typeAsText server) path) (serverTypes, subServer) (clientTypes, subClient)
    (Optional subServer, subClient) ->
      case diffType (InType (typeAsText server) path) (serverTypes, subServer) (clientTypes, subClient) of
        [] -> ifUsedInResponse serverMightNotReturnValueButClientExpectsIt (InType (typeAsText server) path)
        changes -> changes
    (subServer, Optional subClient) ->
      case diffType (InType (typeAsText server) path) (serverTypes, subServer) (clientTypes, subClient) of
        [] -> ifUsedInRequest serverAlwaysExpectsTypeButClientThinksItOptional (InType (typeAsText server) path)
        changes -> changes
    (Unit, Unit) -> []
    (Text, Text) -> []
    (Int, Int) -> []
    (Float, Float) -> []
    (Bool, Bool) -> []
    _ ->
      ifUsedInRequest incompatibleRequestTypes (InType (typeAsText server) path)
        <> ifUsedInResponse incompatibleResponseTypes (InType (typeAsText server) path)

diffCustomType ::
  Path context ->
  (Map.Map Text CustomType, [Constructor]) ->
  (Map.Map Text CustomType, [Constructor]) ->
  [Warning]
diffCustomType path (serverTypes, server) (clientTypes, client) =
  merge
    ( \name _ diffs ->
        ifUsedInResponse clientWontAcceptConstructor (InConstructor name path) <> diffs
    )
    ( \name serverConstructor clientConstructor diffs ->
        diffFields
          (InConstructor name path)
          (serverTypes, fields serverConstructor)
          (clientTypes, fields clientConstructor)
          <> diffs
    )
    ( \name _ diffs ->
        ifUsedInRequest serverWontAcceptConstructor (InConstructor name path) <> diffs
    )
    (constructorTuples server)
    (constructorTuples client)
    []
  where
    constructorTuples constructors =
      fmap
        (\constructor -> (constructorName constructor, constructor))
        constructors

diffFields ::
  Path context ->
  (Map.Map Text CustomType, [Field]) ->
  (Map.Map Text CustomType, [Field]) ->
  [Warning]
diffFields path (serverTypes, server) (clientTypes, client) =
  merge
    ( \name field diffs ->
        case fieldType field of
          Unit -> []
          Optional _ -> []
          List _ -> []
          Dict _ _ -> []
          _ ->
            ifUsedInRequest clientDoesntSendMandatoryField (InField name path) <> diffs
    )
    ( \name serverField clientField diffs ->
        diffType
          (InField name path)
          (serverTypes, fieldType serverField)
          (clientTypes, fieldType clientField)
          <> diffs
    )
    ( \name field diffs ->
        case fieldType field of
          Unit -> []
          Optional _ -> []
          List _ -> []
          Dict _ _ -> []
          _ ->
            ifUsedInResponse serverWontReturnExpectedField (InField name path) <> diffs
    )
    (fieldTuples server)
    (fieldTuples client)
    []
  where
    fieldTuples fields =
      fmap
        (\field -> (fieldName field, field))
        fields

merge ::
  Ord key =>
  (key -> a -> result -> result) ->
  (key -> a -> b -> result -> result) ->
  (key -> b -> result -> result) ->
  [(key, a)] ->
  [(key, b)] ->
  result ->
  result
merge leftOnly both rightOnly left right start =
  mergeHelp start (sortOn fst left) (sortOn fst right)
  where
    mergeHelp acc [] [] = acc
    mergeHelp acc [] ((k, r) : rs) = mergeHelp (rightOnly k r acc) [] rs
    mergeHelp acc ((k, l) : ls) [] = mergeHelp (leftOnly k l acc) ls []
    mergeHelp acc ((kl, l) : ls) ((kr, r) : rs)
      | kl == kr = mergeHelp (both kl l r acc) ls rs
      | kl < kr = mergeHelp (leftOnly kl l acc) ls ((kr, r) : rs)
      | kl > kr = mergeHelp (rightOnly kr r acc) ((kl, l) : ls) rs
      | Prelude.otherwise = error "unreachable"
