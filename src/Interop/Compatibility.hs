{-# LANGUAGE LambdaCase #-}

-- | Find differences between types and explain whether they're backwards
-- compatible or not.
module Interop.Compatibility
  ( checkServerClientCompatibility,
  )
where

import Data.Function ((&))
import Data.List (foldl', sortOn)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Interop.Spec as Spec
import Interop.Wire.Flat

checkServerClientCompatibility :: Spec.Service -> Spec.Service -> Either Text ()
checkServerClientCompatibility server client =
  merge
    (\_ _ acc -> acc) -- server-only endpoints are fine
    ( \endpointName serverEndpoint clientEndpoint acc ->
        let requestTypeWarnings =
              diffType
                (Spec.customTypes server, Spec.requestType serverEndpoint)
                (Spec.customTypes client, Spec.requestType clientEndpoint)
                & requestWarnings (InEndpoint endpointName)
            responseTypeWarnings =
              diffType
                (Spec.customTypes server, Spec.responseType serverEndpoint)
                (Spec.customTypes client, Spec.responseType clientEndpoint)
                & responseWarnings (InEndpoint endpointName)
         in requestTypeWarnings <> responseTypeWarnings <> acc
    )
    ( \_ _ acc ->
        "The client supports an endpoint that the server doesn't. Maybe the endpoint was recently removed from the server. If client code calls the endpoint the server will return an error." :
        acc
    )
    (Map.toList (Spec.endpoints server))
    (Map.toList (Spec.endpoints client))
    []
    & ( \case
          [] -> Right ()
          first : rest ->
            foldl' (\acc entry -> acc <> "\n\n" <> entry) first rest
              & Builder.toLazyText
              & Data.Text.Lazy.toStrict
              & Left
      )

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

data Path
  = InEndpoint Text
  | InType Text Path
  | InConstructor Text Path
  | InField Text Path

warningToText :: Builder.Builder -> Path -> Builder.Builder -> Builder.Builder
warningToText problem path explanation =
  problem <> "\n" <> renderContext path <> "\n\n" <> explanation

renderContext :: Path -> Builder.Builder
renderContext (InField fieldName rest) = renderContext rest <> " { " <> Builder.fromText fieldName <> " }"
renderContext (InConstructor constructorName rest) = renderContext rest <> " = " <> Builder.fromText constructorName
renderContext (InType typeName _) = "data " <> Builder.fromText typeName
renderContext (InEndpoint _) = ""

requestWarnings :: Path -> [TypeDiff] -> [Builder.Builder]
requestWarnings path =
  concatMap $ \case
    TypeMadeOptional _ ->
      []
    TypeMadeNonOptional type_ ->
      [ warningToText
          "A type used in requests is no longer optional."
          (InType (typeAsText type_) path)
          "If any clients are still leaving the type out of requests those will start failing. Make sure clients are always setting this field before going forward with this change."
      ]
    TypeChanged type_ _ ->
      [ warningToText
          "A type used in requests has changed."
          (InType (typeAsText type_) path)
          "This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used."
      ]
    CustomTypeChanged type_ constructorDiffs ->
      constructorRequestWarnings (InType (typeName type_) path) constructorDiffs
    FieldsChanged type_ fieldDiffs ->
      fieldRequestWarnings (InType (typeName type_) path) fieldDiffs

responseWarnings :: Path -> [TypeDiff] -> [Builder.Builder]
responseWarnings path =
  concatMap $ \case
    TypeMadeOptional type_ ->
      [ warningToText
          "A type used responses has been made optional."
          (InType (typeAsText type_) path)
          "Previous versions of the client code will expect the type to always be present and fail if this is not the case. To avoid failures make sure updated clients are deployed before returning Nothing values."
      ]
    TypeMadeNonOptional _ ->
      []
    TypeChanged type_ _ ->
      [ warningToText
          "A type used in responses has changed."
          (InType (typeAsText type_) path)
          "This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used."
      ]
    CustomTypeChanged type_ constructorDiffs ->
      constructorResponseWarnings (InType (typeName type_) path) constructorDiffs
    FieldsChanged type_ fieldDiffs ->
      fieldResponseWarnings (InType (typeName type_) path) fieldDiffs

constructorRequestWarnings :: Path -> NonEmpty ConstructorDiff -> [Builder.Builder]
constructorRequestWarnings path =
  concatMap $ \case
    ConstructorAdded _ ->
      []
    ConstructorRemoved constructor ->
      [ warningToText
          "A constructor was removed from a type used in requests."
          (InConstructor (constructorName constructor) path)
          "Clients that send us requests using the removed constructor will receive an error. Before going forward with this change, make sure clients are no longer using the constructor in requests!"
      ]
    ConstructorChanged constructor fieldDiffs ->
      fieldRequestWarnings
        (InConstructor (constructorName constructor) path)
        fieldDiffs

constructorResponseWarnings :: Path -> NonEmpty ConstructorDiff -> [Builder.Builder]
constructorResponseWarnings path =
  concatMap $ \case
    ConstructorAdded constructor ->
      [ warningToText
          "A constructor was added to a type used in responses."
          (InConstructor (constructorName constructor) path)
          "Using this constructor in responses will cause failures in versions of clients that do not support it yet. Make sure to upgrade those clients before using the new constructor!"
      ]
    ConstructorRemoved _ ->
      []
    ConstructorChanged constructor fieldDiffs ->
      fieldResponseWarnings
        (InConstructor (constructorName constructor) path)
        fieldDiffs

fieldRequestWarnings :: Path -> NonEmpty FieldDiff -> [Builder.Builder]
fieldRequestWarnings path =
  concatMap $ \case
    FieldAdded field ->
      case fieldType field of
        List _ ->
          []
        Optional _ ->
          []
        Dict _ _ ->
          []
        _ ->
          [ warningToText
              "A type used in requests has a mandatory field."
              (InField (fieldName field) path)
              "This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First add an optional field. Then update clients to always set the optional field. Finally make the new field non-optional."
          ]
    FieldRemoved _ ->
      []
    FieldChanged field typeDiffs ->
      requestWarnings
        (InField (fieldName field) path)
        (NonEmpty.toList typeDiffs)

fieldResponseWarnings :: Path -> NonEmpty FieldDiff -> [Builder.Builder]
fieldResponseWarnings path =
  concatMap $ \case
    FieldAdded _ ->
      []
    FieldRemoved field ->
      case fieldType field of
        List _ ->
          []
        Optional _ ->
          []
        Dict _ _ ->
          []
        _ ->
          [ warningToText
              "A type used in responses has lost a mandatory field."
              (InField (fieldName field) path)
              "This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First make this field optional but keep setting it on all responses. Then update clients to support the absence of the field. Finally remove the field."
          ]
    FieldChanged field typeDiffs ->
      responseWarnings
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
    (Dict keyBefore valBefore, Dict keyAfter valAfter) ->
      case diffType (beforeTypes, keyBefore) (afterTypes, keyAfter)
        <> diffType (beforeTypes, valBefore) (afterTypes, valAfter) of
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
