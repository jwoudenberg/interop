{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Interop
  ( Wire,
    Service (..),
    Endpoint (..),
    service,
    convert,
    wai,
  )
where

import qualified Control.Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (groupWith)
import qualified GHC.Stack as Stack
import Interop.Wire (Wire)
import qualified Interop.Wire as Wire
import qualified Interop.Wire.Flat as Flat
import qualified Network.Wai as Wai

data Endpoint m where
  Endpoint :: (Wire req, Wire res) => {name :: Text, fn :: (req -> m res)} -> Endpoint m

data Service m = Service
  { endpoints :: Map.Map Text (Endpoint m),
    customTypes :: [Flat.CustomType]
  }

convert :: (forall a. m a -> n a) -> Service m -> Service n
convert nt service' =
  service'
    { endpoints =
        fmap
          (\(Endpoint name f) -> Endpoint name (nt . f))
          (endpoints service')
    }

data NamingCollision
  = DuplicateType Wire.TypeDefinition Wire.TypeDefinition
  | DuplicateConstructor Text Wire.TypeDefinition Wire.TypeDefinition
  | DuplicateEndpointName Text (Maybe Stack.SrcLoc) (Maybe Stack.SrcLoc)
  deriving (Show)

service :: [Endpoint m] -> Either NamingCollision (Service m)
service endpoints' = do
  customTypes <-
    endpoints'
      & concatMap endpointTypes
      & findCustomTypes
  Right
    Service
      { endpoints =
          endpoints'
            & fmap (\endpoint -> (name endpoint, endpoint))
            & Map.fromList,
        customTypes
      }

findCustomTypes :: [Wire.WireType] -> Either NamingCollision [Flat.CustomType]
findCustomTypes wireTypes =
  let typesByDef = foldr Flat.customTypesByDef Map.empty wireTypes
   in typesByDef
        & Map.toList
        & groupWith (Flat.typeName . snd)
        & traverse
          ( \case
              [] -> error "groupWith returned empty group."
              [(_, customType)] -> Right customType
              (def1, _) : (def2, _) : _ -> Left (DuplicateType def1 def2)
          )

endpointTypes :: Endpoint m -> [Wire.WireType]
endpointTypes (Endpoint _ (_ :: req -> m res)) =
  [ Wire.type_ (Proxy :: Proxy req),
    Wire.type_ (Proxy :: Proxy res)
  ]

data Error
  = ReceivedUnknownCmd Text
  | FailedToParseRequest Text
  deriving (Show)

instance Control.Exception.Exception Error

run :: Monad m => Service m -> ByteString -> (forall a. Error -> m a) -> m ByteString
run service' reqBytes handleErr = do
  (cmd, payload) <-
    case Aeson.eitherDecode reqBytes of
      Left parseErr -> handleErr (FailedToParseRequest (T.pack parseErr))
      Right parsed -> pure parsed
  case Map.lookup cmd (endpoints service') of
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
