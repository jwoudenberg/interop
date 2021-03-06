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
  )
where

import Control.Applicative ((<|>))
import qualified Control.Exception
import qualified Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (foldl')
import qualified Data.Foldable as Foldable
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import Data.List (sortOn)
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (toBoundedInteger, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Word
import qualified GHC.TypeLits
import Interop.Generics
import qualified Network.Wai as Wai

data Endpoint m where
  Endpoint :: (Wire req, Wire res) => (req -> m res) -> Endpoint m

newtype Service m = Service (HM.HashMap Text (Endpoint m))

convert :: (forall a. m a -> n a) -> Service m -> Service n
convert nt (Service endpointMap) =
  endpointMap
    & fmap (\(Endpoint f) -> Endpoint (nt . f))
    & Service

data InvalidService
  = InvalidRequestType WireType
  | DuplicateRequestType Text
  deriving (Show)

service :: [Endpoint m] -> Either InvalidService (Service m)
service endpoints =
  Control.Monad.foldM
    ( \endpointMap endpoint ->
        case name (requestType endpoint) of
          Nothing -> Left (InvalidRequestType (requestType endpoint))
          Just cmdName ->
            if HM.member cmdName endpointMap
              then Left (DuplicateRequestType cmdName)
              else Right (HM.insert cmdName endpoint endpointMap)
    )
    HM.empty
    endpoints
    & fmap Service

name :: WireType -> Maybe Text
name wireType =
  case wireType of
    Constructors typeDefinition _ -> Just (typeName typeDefinition)
    _ -> Nothing

requestType :: Endpoint m -> WireType
requestType (Endpoint (f :: req -> m res)) =
  type_ (Proxy :: Proxy req)

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
  case HM.lookup cmd endpointMap of
    Nothing -> handleErr (ReceivedUnknownCmd cmd)
    Just (Endpoint f) -> do
      case Aeson.parseEither decode payload of
        Left parseErr -> handleErr (FailedToParseRequest (T.pack parseErr))
        Right req -> Encoding.encodingToLazyByteString . encode <$> f req

wai :: Service IO -> Wai.Application
wai service =
  \req respond -> do
    reqBytes <- Wai.strictRequestBody req
    res <- run service reqBytes Control.Exception.throwIO
    respond (Wai.responseLBS (toEnum 200) [] res)

data TypeDiff
  = AddedConstructor Text WireType
  | RemovedConstructor Text WireType
  | ChangedConstructor Text TypeDiff
  | AddedField Text WireType
  | RemovedField Text WireType
  | ChangedField Text TypeDiff
  | ChangedType WireType WireType
  | MadeOptional
  | MadeNonOptional

data Path
  = Type Text
  | Constructor Text Path
  | Field Text Path

diffType :: Path -> WireType -> WireType -> [(Path, TypeDiff)]
diffType path (Constructors name1 ctors1) (Constructors name2 ctors2) = undefined

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
  undefined mergeHelp start (sortOn fst left) (sortOn fst right)
  where
    mergeHelp acc [] ((k, r) : rs) = mergeHelp (rightOnly k r acc) [] rs
    mergeHelp acc ((k, l) : ls) [] = mergeHelp (leftOnly k l acc) ls []
    mergeHelper acc ((kl, l) : ls) ((kr, r) : rs)
      | kl == kr = mergeHelp (both kl l r acc) ls rs
      | kl < kr = mergeHelp (leftOnly kl l acc) ls ((kr, r) : rs)
      | kl > kr = mergeHelp (rightOnly kr r acc) ((kl, l) : ls) rs

saveSpec :: FilePath -> Service m -> IO ()
saveSpec = undefined

loadSpec :: FilePath -> IO (Service Proxy)
loadSpec = undefined

-- | Lists breaking and non-breaking changes in the API.
changelog :: Service m -> Service n -> Text
changelog = undefined
