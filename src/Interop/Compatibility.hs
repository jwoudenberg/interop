{-# LANGUAGE LambdaCase #-}

-- | Find differences between types and explain whether they're backwards
-- compatible or not.
module Interop.Compatibility
  ( checkServerClientCompatibility,
  )
where

import Data.Function ((&))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Interop.Service as Service
import qualified Interop.Wire as Wire
import Interop.Wire.Flat

checkServerClientCompatibility :: Service.Service m -> Service.Service n -> Text
checkServerClientCompatibility server client =
  merge
    (\_ _ acc -> acc) -- server-only endpoints are fine
    ( \endpointName serverEndpoint clientEndpoint acc ->
        let requestTypeWarnings =
              diffType
                (customTypeMap server, requestType serverEndpoint)
                (customTypeMap client, requestType clientEndpoint)
                & typeWarnings (InEndpoint endpointName)
                & filter (\warning -> context warning == InRequest)
            responseTypeWarnings =
              diffType
                (customTypeMap server, responseType serverEndpoint)
                (customTypeMap client, responseType clientEndpoint)
                & typeWarnings (InEndpoint endpointName)
                & filter (\warning -> context warning == InResponse)
         in requestTypeWarnings <> responseTypeWarnings <> acc
    )
    ( \endpointName _ acc ->
        ChangeWarning
          { path = InEndpoint endpointName,
            severity = Warning,
            context = InRequest,
            warning = "The client supports an endpoint that the server doesn't. Maybe the endpoint was recently removed from the server. If client code calls the endpoint the server will return an error."
          } :
        acc
    )
    (Map.toList (Service.endpoints server))
    (Map.toList (Service.endpoints client))
    []
    & fmap warningToText
    & ( \case
          [] -> "No warnings."
          warnings -> Text.intercalate "\n\n" warnings
      )

requestType :: Service.Endpoint m -> Type
requestType (Service.Endpoint _ _ (_ :: req -> m res)) =
  Wire.type_ (Proxy :: Proxy req)
    & fromFieldType

responseType :: Service.Endpoint m -> Type
responseType (Service.Endpoint _ _ (_ :: req -> m res)) =
  Wire.type_ (Proxy :: Proxy res)
    & fromFieldType

customTypeMap :: Service.Service m -> Map.Map Text CustomType
customTypeMap service' =
  Service.customTypes service'
    & fmap (\customType -> (typeName customType, customType))
    & Map.fromList

data TypeDiff
  = CustomTypeChanged CustomType (NonEmpty ConstructorDiff)
  | FieldsChanged CustomType (NonEmpty FieldDiff)
  | TypeMadeOptional Type
  | TypeMadeNonOptional Type
  | TypeChanged Type Type

data ConstructorDiff
  = ConstructorAdded Constructor
  | ConstructorRemoved Constructor
  | ConstructorChanged Constructor (NonEmpty FieldDiff)

data FieldDiff
  = FieldAdded Field
  | FieldRemoved Field
  | FieldChanged Field (NonEmpty TypeDiff)

data ChangeWarning = ChangeWarning
  { path :: Path,
    severity :: Severity,
    context :: Context,
    warning :: Text
  }

data Path
  = InEndpoint Text
  | InType Text Path
  | InConstructor Text Path
  | InField Text Path

data Severity = Warning | Error

data Context = InRequest | InResponse
  deriving (Eq)

warningToText :: ChangeWarning -> Text
warningToText typeChangeWarning =
  pathToText (path typeChangeWarning)
    <> "\n"
    <> severityToText (severity typeChangeWarning)
    <> ": "
    <> warning typeChangeWarning

severityToText :: Severity -> Text
severityToText Warning = "Warning"
severityToText Error = "Error"

pathToText :: Path -> Text
pathToText (InEndpoint endpointName) = "In endpoint: " <> endpointName
pathToText (InType typeName rest) = pathToText rest <> ", in type: " <> typeName
pathToText (InConstructor constructorName rest) = pathToText rest <> ", in constructor: " <> constructorName
pathToText (InField fieldName rest) = pathToText rest <> ", in field: " <> fieldName

typeWarnings :: Path -> [TypeDiff] -> [ChangeWarning]
typeWarnings path =
  concatMap $ \case
    TypeMadeOptional type_ ->
      [ ChangeWarning
          { path = InType (typeAsText type_) path,
            severity = Warning,
            context = InResponse,
            warning = "A response type has been made optional. Previous versions of the client code will expect the type to always be present and fail if this is not the case. To avoid failures make sure updated clients are deployed before returning Nothing values."
          }
      ]
    TypeMadeNonOptional type_ ->
      [ ChangeWarning
          { path = InType (typeAsText type_) path,
            severity = Warning,
            context = InRequest,
            warning = "A request type was optional before but no longer is. If any clients are still leaving the type out of requests those will start failing. Make sure clients are always setting this field before going forward with this change."
          }
      ]
    TypeChanged type_ _ ->
      [ ChangeWarning
          { path = InType (typeAsText type_) path,
            severity = Error,
            context = InRequest,
            warning = "We're expecting an entirely different request type. This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used."
          },
        ChangeWarning
          { path = InType (typeAsText type_) path,
            severity = Error,
            context = InResponse,
            warning = "We're returning an entirely different response type. This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used."
          }
      ]
    CustomTypeChanged type_ constructorDiffs ->
      constructorWarnings (InType (typeName type_) path) constructorDiffs
    FieldsChanged type_ fieldDiffs ->
      fieldWarnings (InType (typeName type_) path) fieldDiffs

constructorWarnings :: Path -> NonEmpty ConstructorDiff -> [ChangeWarning]
constructorWarnings path =
  concatMap $ \case
    ConstructorAdded constructor ->
      [ ChangeWarning
          { path = InConstructor (constructorName constructor) path,
            severity = Warning,
            context = InResponse,
            warning = "A constructor was added to a response type. Using this constructor in responses will cause failures in versions of clients that do not support it yet. Make sure to upgrade those clients before using the new constructor!"
          }
      ]
    ConstructorRemoved constructor ->
      [ ChangeWarning
          { path = InConstructor (constructorName constructor) path,
            severity = Warning,
            context = InRequest,
            warning = "A constructor was removed from a request type. Clients that send us requests using the removed constructor will receive an error. Before going forward with this change, make sure clients are no longer using the constructor in requests!"
          }
      ]
    ConstructorChanged constructor fieldDiffs ->
      fieldWarnings
        (InConstructor (constructorName constructor) path)
        fieldDiffs

fieldWarnings :: Path -> NonEmpty FieldDiff -> [ChangeWarning]
fieldWarnings path =
  concatMap $ \case
    FieldAdded field ->
      case fieldType field of
        List _ ->
          []
        Optional _ ->
          []
        _ ->
          [ ChangeWarning
              { path = InField (fieldName field) path,
                severity = Error,
                context = InRequest,
                warning = "A non-optional field was added to a request type. This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First add an optional field. Then update clients to always set the optional field. Finally make the new field non-optional."
              }
          ]
    FieldRemoved field ->
      case fieldType field of
        List _ ->
          []
        Optional _ ->
          []
        _ ->
          [ ChangeWarning
              { path = InField (fieldName field) path,
                severity = Error,
                context = InResponse,
                warning = "A non-optional field was removed from a response type. This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First make this field optional but keep setting it on all responses. Then update clients to support the absence of the field. Finally remove the field."
              }
          ]
    FieldChanged field typeDiffs ->
      typeWarnings
        (InField (fieldName field) path)
        (NonEmpty.toList typeDiffs)

diffType ::
  (Map.Map Text CustomType, Type) ->
  (Map.Map Text CustomType, Type) ->
  [TypeDiff]
diffType (beforeTypes, before) (afterTypes, after) =
  case (before, after) of
    (NestedCustomType beforeName, NestedCustomType afterName) -> do
      beforeType <- maybe [] pure (Map.lookup beforeName beforeTypes)
      afterType <- maybe [] pure (Map.lookup afterName afterTypes)
      case (subTypes beforeType, subTypes afterType) of
        (Right beforeConstructors, Right afterConstructors) ->
          diffCustomType
            (beforeTypes, beforeConstructors)
            (afterTypes, afterConstructors)
            & nonEmpty
            & maybe [] (pure . CustomTypeChanged beforeType)
        (Left beforeFields, Left afterFields) ->
          diffFields
            (beforeTypes, beforeFields)
            (afterTypes, afterFields)
            & nonEmpty
            & maybe [] (pure . FieldsChanged beforeType)
        (_, _) ->
          [TypeChanged before after]
    (List subBefore, List subAfter) ->
      case diffType (beforeTypes, subBefore) (afterTypes, subAfter) of
        [] -> []
        _ -> [TypeChanged before after]
    (Optional subBefore, Optional subAfter) ->
      case diffType (beforeTypes, subBefore) (afterTypes, subAfter) of
        [] -> []
        _ -> [TypeChanged before after]
    (Optional subBefore, subAfter) ->
      case diffType (beforeTypes, subBefore) (afterTypes, subAfter) of
        [] -> [TypeMadeNonOptional after]
        _ -> [TypeChanged before after]
    (subBefore, Optional subAfter) ->
      case diffType (beforeTypes, subBefore) (afterTypes, subAfter) of
        [] -> [TypeMadeOptional before]
        _ -> [TypeChanged before after]
    (Unit, Unit) -> []
    (Text, Text) -> []
    (Int, Int) -> []
    (Float, Float) -> []
    (Bool, Bool) -> []
    _ -> [TypeChanged before after]

diffCustomType ::
  (Map.Map Text CustomType, [Constructor]) ->
  (Map.Map Text CustomType, [Constructor]) ->
  [ConstructorDiff]
diffCustomType (beforeTypes, before) (afterTypes, after) =
  merge
    (\_ constructor diffs -> ConstructorRemoved constructor : diffs)
    ( \_ beforeConstructor afterConstructor diffs ->
        diffFields
          (beforeTypes, fields beforeConstructor)
          (afterTypes, fields afterConstructor)
          & nonEmpty
          & maybe diffs ((: diffs) . ConstructorChanged beforeConstructor)
    )
    (\_ constructor diffs -> ConstructorAdded constructor : diffs)
    (constructorTuples before)
    (constructorTuples after)
    []
  where
    constructorTuples constructors =
      fmap
        (\constructor -> (constructorName constructor, constructor))
        constructors

diffFields ::
  (Map.Map Text CustomType, [Field]) ->
  (Map.Map Text CustomType, [Field]) ->
  [FieldDiff]
diffFields (beforeTypes, before) (afterTypes, after) =
  merge
    (\_ field diffs -> FieldRemoved field : diffs)
    ( \_ beforeField afterField diffs ->
        diffType
          (beforeTypes, fieldType beforeField)
          (afterTypes, fieldType afterField)
          & nonEmpty
          & maybe diffs ((: diffs) . FieldChanged beforeField)
    )
    (\_ field diffs -> FieldAdded field : diffs)
    (fieldTuples before)
    (fieldTuples after)
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
