{-# LANGUAGE LambdaCase #-}

-- | Find differences between types and explain whether they're backwards
-- compatible or not.
module Interop.Diff
  ( diffType,
    checkBackwardsCompatibility,
  )
where

import Data.Function ((&))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Interop.Wire.Flat

data TypeDiff
  = CustomTypeChanged CustomType (NonEmpty ConstructorDiff)
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

data TypeChangeWarning = TypeChangeWarning
  { path :: Path,
    severity :: Severity,
    warning :: Warning
  }

data Path
  = InType Text Path
  | InConstructor Text Path
  | InField Text Path
  | Root

data Severity = Warning | Error

data Warning
  = TypeInRequest Text
  | TypeInResponse Text

checkBackwardsCompatibility :: [TypeDiff] -> [TypeChangeWarning]
checkBackwardsCompatibility = typeWarnings Root

typeWarnings :: Path -> [TypeDiff] -> [TypeChangeWarning]
typeWarnings path =
  concatMap $ \case
    TypeMadeOptional type_ ->
      [ TypeChangeWarning
          { path = InType (typeAsText type_) path,
            severity = Warning,
            warning = TypeInResponse "A response type has been made optional. Previous versions of the client code will expect the type to always be present and fail if this is not the case. To avoid failures make sure updated clients are deployed before returning Nothing values."
          }
      ]
    TypeMadeNonOptional type_ ->
      [ TypeChangeWarning
          { path = InType (typeAsText type_) path,
            severity = Warning,
            warning = TypeInRequest "A request type was optional before but no longer is. If any clients are still leaving the type out of requests those will start failing. Make sure clients are always setting this field before going forward with this change."
          }
      ]
    TypeChanged type_ _ ->
      [ TypeChangeWarning
          { path = InType (typeAsText type_) path,
            severity = Error,
            warning = TypeInRequest "We're expecting an entirely different request type. This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used."
          },
        TypeChangeWarning
          { path = InType (typeAsText type_) path,
            severity = Error,
            warning = TypeInResponse "We're returning an entirely different response type. This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used."
          }
      ]
    CustomTypeChanged type_ constructorDiffs ->
      constructorWarnings (InType (typeName type_) path) constructorDiffs

constructorWarnings :: Path -> NonEmpty ConstructorDiff -> [TypeChangeWarning]
constructorWarnings path =
  concatMap $ \case
    ConstructorAdded constructor ->
      [ TypeChangeWarning
          { path = InConstructor (constructorName constructor) path,
            severity = Warning,
            warning = TypeInResponse "A constructor was added to a response type. Using this constructor in responses will cause failures in versions of clients that do not support it yet. Make sure to upgrade those clients before using the new constructor!"
          }
      ]
    ConstructorRemoved constructor ->
      [ TypeChangeWarning
          { path = InConstructor (constructorName constructor) path,
            severity = Warning,
            warning = TypeInRequest "A constructor was removed from a request type. Clients that send us requests using the removed constructor will receive an error. Before going forward with this change, make sure clients are no longer using the constructor in requests!"
          }
      ]
    ConstructorChanged constructor fieldDiffs ->
      fieldWarnings
        (InConstructor (constructorName constructor) path)
        fieldDiffs

fieldWarnings :: Path -> NonEmpty FieldDiff -> [TypeChangeWarning]
fieldWarnings path =
  concatMap $ \case
    FieldAdded field ->
      case fieldType field of
        Optional _ ->
          []
        _ ->
          [ TypeChangeWarning
              { path = InField (fieldName field) path,
                severity = Error,
                warning = TypeInRequest "A non-optional field was added to a request type. This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First add an optional field. Then update clients to always set the optional field. Finally make the new field non-optional."
              }
          ]
    FieldRemoved field ->
      case fieldType field of
        Optional _ ->
          []
        _ ->
          [ TypeChangeWarning
              { path = InField (fieldName field) path,
                severity = Error,
                warning = TypeInResponse "A non-optional field was removed from a response type. This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First make this field optional but keep setting it on all responses. Then update clients to support the absence of the field. Finally remove the field."
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
      diffCustomType
        (beforeTypes, beforeType)
        (afterTypes, afterType)
        & nonEmpty
        & maybe [] (pure . CustomTypeChanged beforeType)
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
  (Map.Map Text CustomType, CustomType) ->
  (Map.Map Text CustomType, CustomType) ->
  [ConstructorDiff]
diffCustomType (beforeTypes, before) (afterTypes, after) =
  merge
    (\_ constructor diffs -> ConstructorRemoved constructor : diffs)
    ( \_ beforeConstructor afterConstructor diffs ->
        diffConstructor
          (beforeTypes, beforeConstructor)
          (afterTypes, afterConstructor)
          & nonEmpty
          & maybe diffs ((: diffs) . ConstructorChanged beforeConstructor)
    )
    (\_ constructor diffs -> ConstructorAdded constructor : diffs)
    (constructorTuples before)
    (constructorTuples after)
    []
  where
    constructorTuples customType =
      fmap
        (\constructor -> (constructorName constructor, constructor))
        (constructors customType)

diffConstructor ::
  (Map.Map Text CustomType, Constructor) ->
  (Map.Map Text CustomType, Constructor) ->
  [FieldDiff]
diffConstructor (beforeTypes, before) (afterTypes, after) =
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
    fieldTuples customType =
      fmap
        (\field -> (fieldName field, field))
        (fields customType)

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