{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Interop
  ( Wire,
    Service (..),
    Endpoint (..),
    service,
    convert,
    wai,
    customTypes,
  )
where

import qualified Control.Exception
import qualified Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Interop.Wire (Wire)
import qualified Interop.Wire as Wire
import Interop.Wire.Flat
import qualified Network.Wai as Wai

data Endpoint m where
  Endpoint :: (Wire req, Wire res) => {name :: Text, fn :: (req -> m res)} -> Endpoint m

newtype Service m = Service (Map.Map Text (Endpoint m))

convert :: (forall a. m a -> n a) -> Service m -> Service n
convert nt (Service endpointMap) =
  endpointMap
    & fmap (\(Endpoint name f) -> Endpoint name (nt . f))
    & Service

data InvalidService
  = DuplicateRequestType Text
  deriving (Show)

service :: [Endpoint m] -> Either InvalidService (Service m)
service endpoints =
  Control.Monad.foldM
    ( \endpointMap endpoint ->
        if Map.member (name endpoint) endpointMap
          then Left (DuplicateRequestType (name endpoint))
          else Right (Map.insert (name endpoint) endpoint endpointMap)
    )
    Map.empty
    endpoints
    & fmap Service

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
    Just (Endpoint _ f) -> do
      case Aeson.parseEither Wire.decode payload of
        Left parseErr -> handleErr (FailedToParseRequest (T.pack parseErr))
        Right req -> Encoding.encodingToLazyByteString . Wire.encode <$> f req

wai :: Service IO -> Wai.Application
wai service' =
  \req respond -> do
    reqBytes <- Wai.strictRequestBody req
    res <- run service' reqBytes Control.Exception.throwIO
    respond (Wai.responseLBS (toEnum 200) [] res)
