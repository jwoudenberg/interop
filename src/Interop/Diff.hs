-- | Find differences between types and explain whether they're backwards
-- compatible or not.
module Interop.Diff
  ( TypeDiff (..),
    diffType,
  )
where

import Data.Function ((&))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
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
