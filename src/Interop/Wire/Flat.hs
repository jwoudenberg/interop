{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A flat, non-recursive version of the type defined in the Wire module.
-- This is easier to work with for code generation.
module Interop.Wire.Flat
  ( fromFieldType,
    CustomType (..),
    Constructor (..),
    Field (..),
    Type (..),
    typeAsText,
    customTypesByDef,
  )
where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

-- | This type is like Interop.Wire.WireType, except it's not recursive.
-- It defines a single `newtype` or `data` declaration. If one of the parameters
-- of this type is another custom type then reference it by name using the
-- `NestedCustomType` constructor instead of embedding it entirely.
data CustomType = CustomType
  { typeName :: Text,
    subTypes :: Either [Field] [Constructor]
  }
  deriving (Generic)

instance Aeson.ToJSON CustomType

instance Aeson.FromJSON CustomType

data Constructor = Constructor
  { constructorName :: Text,
    fields :: [Field]
  }
  deriving (Generic)

instance Aeson.ToJSON Constructor

instance Aeson.FromJSON Constructor

data Field = Field
  { fieldName :: Text,
    fieldType :: Type
  }
  deriving (Generic)

instance Aeson.ToJSON Field

instance Aeson.FromJSON Field

data Type
  = Optional Type
  | List Type
  | Dict Type Type
  | Text
  | Int
  | Float
  | Bool
  | Unit
  | NestedCustomType Text
  deriving (Generic)

instance Aeson.ToJSON Type

instance Aeson.FromJSON Type

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
    Dict keyType valType -> "Dict " <> typeAsText keyType <> " " <> typeAsText valType
    NestedCustomType name -> name

customTypesByDef ::
  Wire.WireType ->
  Map.Map Wire.TypeDefinition CustomType ->
  Map.Map Wire.TypeDefinition CustomType
customTypesByDef wireType acc =
  case wireType of
    Wire.List subType -> customTypesByDef subType acc
    Wire.Optional subType -> customTypesByDef subType acc
    Wire.Dict keyType valType -> customTypesByDef valType (customTypesByDef keyType acc)
    Wire.Unit -> acc
    Wire.Text -> acc
    Wire.Int -> acc
    Wire.Float -> acc
    Wire.Bool -> acc
    Wire.Type def subTypes ->
      -- We bail if we've already seen this type, so recursive types don't send
      -- us into an infinite loop.
      if Map.member def acc
        then acc
        else case subTypes of
          Left wireFields ->
            foldl'
              ( \acc'' field ->
                  customTypesByDef (Wire.fieldType field) acc''
              )
              (Map.insert def (fromWireType def (Left wireFields)) acc)
              wireFields
          Right wireConstructors ->
            foldl'
              ( \acc' constructor ->
                  foldl'
                    ( \acc'' field ->
                        customTypesByDef (Wire.fieldType field) acc''
                    )
                    acc'
                    (Wire.fields constructor)
              )
              (Map.insert def (fromWireType def (Right wireConstructors)) acc)
              wireConstructors

fromWireType :: Wire.TypeDefinition -> Either [Wire.Field] [Wire.Constructor] -> CustomType
fromWireType def wireConstructors =
  CustomType
    { typeName = Wire.typeName def,
      subTypes = bimap (fmap fromWireField) (fmap fromWireConstructor) wireConstructors
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
    Wire.List subType -> List (fromFieldType subType)
    Wire.Optional subType -> Optional (fromFieldType subType)
    Wire.Dict keyType valType -> Dict (fromFieldType keyType) (fromFieldType valType)
    Wire.Unit -> Unit
    Wire.Text -> Text
    Wire.Int -> Int
    Wire.Float -> Float
    Wire.Bool -> Bool
