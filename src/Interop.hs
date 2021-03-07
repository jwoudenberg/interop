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
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Interop.Generics (Wire)
import qualified Interop.Generics as Generics
import Interop.Types
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
