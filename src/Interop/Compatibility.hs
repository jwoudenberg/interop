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
        endpointRemovedFromServer (InEndpoint endpointName) : acc
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

requestTypeMadeNonOptional :: Path 'Request -> Warning
requestTypeMadeNonOptional path =
  Warning
    { short = "A type used in requests is no longer optional.",
      context = path,
      detailed = "If any clients are still leaving the type out of requests those will start failing. Make sure clients are always setting this field before going forward with this change."
    }

requestTypeChanged :: Path 'Request -> Warning
requestTypeChanged path =
  Warning
    { short = "A type used in requests has changed.",
      context = path,
      detailed = "This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used."
    }

responseTypeMadeOptional :: Path 'Response -> Warning
responseTypeMadeOptional path =
  Warning
    { short = "A type used responses has been made optional.",
      context = path,
      detailed = "Previous versions of the client code will expect the type to always be present and fail if this is not the case. To avoid failures make sure updated clients are deployed before returning Nothing values."
    }

responseTypeChanged :: Path 'Response -> Warning
responseTypeChanged path =
  Warning
    { short = "A type used in responses has changed.",
      context = path,
      detailed = "This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used."
    }

requestConstructorRemoved :: Path 'Request -> Warning
requestConstructorRemoved path =
  Warning
    { short = "A constructor was removed from a type used in requests.",
      context = path,
      detailed = "Clients that send us requests using the removed constructor will receive an error. Before going forward with this change, make sure clients are no longer using the constructor in requests!"
    }

responseConstructorAdded :: Path 'Response -> Warning
responseConstructorAdded path =
  Warning
    { short = "A constructor was added to a type used in responses.",
      context = path,
      detailed = "Using this constructor in responses will cause failures in versions of clients that do not support it yet. Make sure to upgrade those clients before using the new constructor!"
    }

mandatoryFieldAddedToRequest :: Path 'Request -> Warning
mandatoryFieldAddedToRequest path =
  Warning
    { short = "A type used in requests has a mandatory field.",
      context = path,
      detailed = "This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First add an optional field. Then update clients to always set the optional field. Finally make the new field non-optional."
    }

mandatoryFieldRemovedFromResponse :: Path 'Response -> Warning
mandatoryFieldRemovedFromResponse path =
  Warning
    { short = "A type used in responses has lost a mandatory field.",
      context = path,
      detailed = "This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First make this field optional but keep setting it on all responses. Then update clients to support the absence of the field. Finally remove the field."
    }

endpointRemovedFromServer :: Path 'Endpoint -> Warning
endpointRemovedFromServer path =
  Warning
    { short = "client uses endpoint unsupported by server",
      context = path,
      detailed = "The client supports an endpoint that the server doesn't. Maybe the endpoint was recently removed from the server. If client code calls the endpoint the server will return an error."
    }

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
          ifUsedInResponse responseTypeChanged (InType (typeAsText server) path)
    (List subBefore, List subAfter) ->
      diffType (InType (typeAsText server) path) (serverTypes, subBefore) (clientTypes, subAfter)
    (Dict keyBefore valBefore, Dict keyAfter valAfter) ->
      diffType (InType (typeAsText server) path) (serverTypes, keyBefore) (clientTypes, keyAfter)
        <> diffType (InType (typeAsText server) path) (serverTypes, valBefore) (clientTypes, valAfter)
    (Optional subBefore, Optional subAfter) ->
      diffType (InType (typeAsText server) path) (serverTypes, subBefore) (clientTypes, subAfter)
    (Optional subBefore, subAfter) ->
      case diffType (InType (typeAsText server) path) (serverTypes, subBefore) (clientTypes, subAfter) of
        [] -> ifUsedInRequest requestTypeMadeNonOptional (InType (typeAsText server) path)
        changes -> changes
    (subBefore, Optional subAfter) ->
      case diffType (InType (typeAsText server) path) (serverTypes, subBefore) (clientTypes, subAfter) of
        [] -> ifUsedInResponse responseTypeMadeOptional (InType (typeAsText server) path)
        changes -> changes
    (Unit, Unit) -> []
    (Text, Text) -> []
    (Int, Int) -> []
    (Float, Float) -> []
    (Bool, Bool) -> []
    _ ->
      ifUsedInRequest requestTypeChanged (InType (typeAsText server) path)
        <> ifUsedInResponse responseTypeChanged (InType (typeAsText server) path)

diffCustomType ::
  Path context ->
  (Map.Map Text CustomType, [Constructor]) ->
  (Map.Map Text CustomType, [Constructor]) ->
  [Warning]
diffCustomType path (serverTypes, server) (clientTypes, client) =
  merge
    ( \name _ diffs ->
        ifUsedInRequest requestConstructorRemoved (InConstructor name path) <> diffs
    )
    ( \name serverConstructor clientConstructor diffs ->
        diffFields
          (InConstructor name path)
          (serverTypes, fields serverConstructor)
          (clientTypes, fields clientConstructor)
          <> diffs
    )
    ( \name _ diffs ->
        ifUsedInResponse responseConstructorAdded (InConstructor name path) <> diffs
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
            ifUsedInResponse mandatoryFieldRemovedFromResponse (InField name path) <> diffs
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
            ifUsedInRequest mandatoryFieldAddedToRequest (InField name path) <> diffs
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
