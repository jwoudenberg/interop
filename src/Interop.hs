{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Interop
  ( Wire,
    Service (..),
    Endpoint (..),
    service,
    convert,
    wai,
    customTypes,

    -- * Type diffing
    TypeDiff (..),
    Path (..),
    diffType,
    merge,
  )
where

import qualified Control.Exception
import qualified Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (groupWith)
import Interop.Generics (Wire)
import qualified Interop.Generics as Generics
import qualified Network.Wai as Wai

data Endpoint m where
  Endpoint :: (Wire req, Wire res) => (req -> m res) -> Endpoint m

newtype Service m = Service (Map.Map Text (Endpoint m))

convert :: (forall a. m a -> n a) -> Service m -> Service n
convert nt (Service endpointMap) =
  endpointMap
    & fmap (\(Endpoint f) -> Endpoint (nt . f))
    & Service

data InvalidService
  = InvalidRequestType Generics.WireType
  | DuplicateRequestType Text
  deriving (Show)

service :: [Endpoint m] -> Either InvalidService (Service m)
service endpoints =
  Control.Monad.foldM
    ( \endpointMap endpoint ->
        case name (requestType endpoint) of
          Nothing -> Left (InvalidRequestType (requestType endpoint))
          Just cmdName ->
            if Map.member cmdName endpointMap
              then Left (DuplicateRequestType cmdName)
              else Right (Map.insert cmdName endpoint endpointMap)
    )
    Map.empty
    endpoints
    & fmap Service

name :: Generics.WireType -> Maybe Text
name wireType =
  case wireType of
    Generics.Type typeDefinition _ -> Just (Generics.typeName typeDefinition)
    _ -> Nothing

requestType :: Endpoint m -> Generics.WireType
requestType (Endpoint (_ :: req -> m res)) =
  Generics.type_ (Proxy :: Proxy req)

data Error
  = ReceivedUnknownCmd Text
  | FailedToParseRequest Text
  deriving (Show)

instance Control.Exception.Exception Error

run :: Monad m => Service m -> ByteString -> (forall a. Error -> m a) -> m ByteString
run (Service endpointMap) reqBytes handleErr = do
  (cmd, payload) <-
    case Aeson.eitherDecode reqBytes of
      Left parseErr -> handleErr (FailedToParseRequest (T.pack parseErr))
      Right parsed -> pure parsed
  case Map.lookup cmd endpointMap of
    Nothing -> handleErr (ReceivedUnknownCmd cmd)
    Just (Endpoint f) -> do
      case Aeson.parseEither Generics.decode payload of
        Left parseErr -> handleErr (FailedToParseRequest (T.pack parseErr))
        Right req -> Encoding.encodingToLazyByteString . Generics.encode <$> f req

wai :: Service IO -> Wai.Application
wai service' =
  \req respond -> do
    reqBytes <- Wai.strictRequestBody req
    res <- run service' reqBytes Control.Exception.throwIO
    respond (Wai.responseLBS (toEnum 200) [] res)

data TypeDiff
  = AddedConstructor Text Generics.WireType
  | RemovedConstructor Text Generics.WireType
  | ChangedConstructor Text TypeDiff
  | AddedField Text Generics.WireType
  | RemovedField Text Generics.WireType
  | ChangedField Text TypeDiff
  | ChangedType Generics.WireType Generics.WireType
  | MadeOptional
  | MadeNonOptional

data Path
  = InType Text
  | InConstructor Text Path
  | InField Text Path

diffType :: Path -> Generics.WireType -> Generics.WireType -> [(Path, TypeDiff)]
diffType _ _ _ = undefined

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
    fieldType :: FieldType
  }

data FieldType
  = Optional FieldType
  | List FieldType
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
            (Map.insert def customType acc)
            wireConstructors
      where
        customType =
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

fromFieldType :: Generics.WireType -> FieldType
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
