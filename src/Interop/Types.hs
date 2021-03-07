{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Interop.Types
  ( customTypes,
    CustomType (..),
    Constructor (..),
    Field (..),
    Type (..),
  )
where

import Data.Function ((&))
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Exts (groupWith)
import qualified Interop.Generics as Generics

-- | This type is like Interop.Generics.WireType, except it's not recursive.
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
  | NestedCustomType Text

customTypes :: Generics.WireType -> Either Text [CustomType]
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
  Generics.WireType ->
  Map.Map Generics.TypeDefinition CustomType ->
  Map.Map Generics.TypeDefinition CustomType
customTypesByDef wireType acc =
  case wireType of
    Generics.List subType -> customTypesByDef subType acc
    Generics.Optional subType -> customTypesByDef subType acc
    Generics.Unit -> acc
    Generics.Text -> acc
    Generics.Int -> acc
    Generics.Float -> acc
    Generics.Bool -> acc
    Generics.Type def wireConstructors ->
      -- We bail if we've already seen this type, so recursive types don't send
      -- us into an infinite loop.
      if Map.member def acc
        then acc
        else
          foldl'
            ( \acc' constructor ->
                foldl'
                  ( \acc'' field ->
                      customTypesByDef (Generics.fieldType field) acc''
                  )
                  acc'
                  (Generics.fields constructor)
            )
            (Map.insert def (fromWireType def wireConstructors) acc)
            wireConstructors

fromWireType :: Generics.TypeDefinition -> [Generics.Constructor] -> CustomType
fromWireType def wireConstructors =
  CustomType
    { typeName = Generics.typeName def,
      constructors = fmap fromWireConstructor wireConstructors
    }

fromWireConstructor :: Generics.Constructor -> Constructor
fromWireConstructor constructor =
  Constructor
    { constructorName = Generics.constructorName constructor,
      fields = fmap fromWireField (Generics.fields constructor)
    }

fromWireField :: Generics.Field -> Field
fromWireField field =
  Field
    { fieldName = Generics.fieldName field,
      fieldType = fromFieldType (Generics.fieldType field)
    }

fromFieldType :: Generics.WireType -> Type
fromFieldType fieldType =
  case fieldType of
    Generics.Type nestedDef _ -> NestedCustomType (Generics.typeName nestedDef)
    Generics.List subType -> List (fromFieldType subType)
    Generics.Optional subType -> Optional (fromFieldType subType)
    Generics.Unit -> Unit
    Generics.Text -> Text
    Generics.Int -> Int
    Generics.Float -> Float
    Generics.Bool -> Bool
