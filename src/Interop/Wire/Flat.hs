-- | A flat, non-recursive version of the type defined in the Wire module.
-- This is easier to work with for code generation.
module Interop.Wire.Flat
  ( customTypes,
    CustomType (..),
    Constructor (..),
    Field (..),
    Type (..),
    typeAsText,
  )
where

import Data.Function ((&))
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (groupWith)
import qualified Interop.Wire as Wire

-- | This type is like Interop.Wire.WireType, except it's not recursive.
-- It defines a single `newtype` or `data` declaration. If one of the parameters
-- of this type is another custom type then reference it by name using the
-- `NestedCustomType` constructor instead of embedding it entirely.
data CustomType = CustomType
  { typeName :: Text,
    constructors :: [Constructor]
  }

data Constructor = Constructor
  { constructorName :: Text,
    fields :: [Field]
  }

data Field = Field
  { fieldName :: Text,
    fieldType :: Type
  }

data Type
  = Optional Type
  | List Type
  | Text
  | Int
  | Float
  | Bool
  | Unit
  | Record [Field]
  | NestedCustomType Text

typeAsText :: Type -> Text
typeAsText type_ =
  case type_ of
    Text -> "Text"
    Int -> "Int"
    Float -> "Float"
    Bool -> "Bool"
    Unit -> "Unit"
    Optional subType -> "Maybe " <> typeAsText subType
    List subType -> "List " <> typeAsText subType
    Record fields ->
      let fieldAsText field =
            fieldName field <> " : " <> typeAsText (fieldType field)
       in "{ " <> T.intercalate ", " (fieldAsText <$> fields) <> " }"
    NestedCustomType name -> name

customTypes :: Wire.WireType -> Either Text [CustomType]
customTypes wireType =
  case withDuplicateNames of
    [] -> Right (Map.elems typesByDef)
    _ -> Left "Some types share the same name and I don't support that, <add more error detail>"
  where
    typesByDef = customTypesByDef wireType Map.empty
    withDuplicateNames =
      typesByDef
        & Map.toList
        & groupWith (typeName . snd)
        & filter ((>= 2) . length)

customTypesByDef ::
  Wire.WireType ->
  Map.Map Wire.TypeDefinition CustomType ->
  Map.Map Wire.TypeDefinition CustomType
customTypesByDef wireType acc =
  case wireType of
    Wire.Record fields -> foldl' (flip customTypesByDef) acc (Wire.fieldType <$> fields)
    Wire.List subType -> customTypesByDef subType acc
    Wire.Optional subType -> customTypesByDef subType acc
    Wire.Unit -> acc
    Wire.Text -> acc
    Wire.Int -> acc
    Wire.Float -> acc
    Wire.Bool -> acc
    Wire.Type def wireConstructors ->
      -- We bail if we've already seen this type, so recursive types don't send
      -- us into an infinite loop.
      if Map.member def acc
        then acc
        else
          foldl'
            ( \acc' constructor ->
                foldl'
                  ( \acc'' field ->
                      customTypesByDef (Wire.fieldType field) acc''
                  )
                  acc'
                  (Wire.fields constructor)
            )
            (Map.insert def (fromWireType def wireConstructors) acc)
            wireConstructors

fromWireType :: Wire.TypeDefinition -> [Wire.Constructor] -> CustomType
fromWireType def wireConstructors =
  CustomType
    { typeName = Wire.typeName def,
      constructors = fmap fromWireConstructor wireConstructors
    }

fromWireConstructor :: Wire.Constructor -> Constructor
fromWireConstructor constructor =
  Constructor
    { constructorName = Wire.constructorName constructor,
      fields = fmap fromWireField (Wire.fields constructor)
    }

fromWireField :: Wire.Field -> Field
fromWireField field =
  Field
    { fieldName = Wire.fieldName field,
      fieldType = fromFieldType (Wire.fieldType field)
    }

fromFieldType :: Wire.WireType -> Type
fromFieldType fieldType =
  case fieldType of
    Wire.Type nestedDef _ -> NestedCustomType (Wire.typeName nestedDef)
    Wire.Record fields ->
      fields
        & fmap (\field -> Field (Wire.fieldName field) (fromFieldType (Wire.fieldType field)))
        & Record
    Wire.List subType -> List (fromFieldType subType)
    Wire.Optional subType -> Optional (fromFieldType subType)
    Wire.Unit -> Unit
    Wire.Text -> Text
    Wire.Int -> Int
    Wire.Float -> Float
    Wire.Bool -> Bool
