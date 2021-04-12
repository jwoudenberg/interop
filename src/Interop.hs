{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Interop
  ( Wire,
    Service (..),
    Endpoint (..),
    endpoint,
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
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty ((:|)), groupWith)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Stack as Stack
import Interop.Wire (Wire)
import qualified Interop.Wire as Wire
import qualified Interop.Wire.Flat as Flat
import qualified Network.Wai as Wai

data Endpoint m where
  Endpoint ::
    (Wire req, Wire res) =>
    { name :: Text,
      srcLoc :: Maybe Stack.SrcLoc,
      fn :: (req -> m res)
    } ->
    Endpoint m

endpoint ::
  (Stack.HasCallStack, Wire req, Wire res) =>
  Text ->
  (req -> m res) ->
  Endpoint m
endpoint name fn =
  Endpoint
    { name,
      fn,
      srcLoc = Stack.withFrozenCallStack stackFrame
    }

stackFrame :: Stack.HasCallStack => Maybe Stack.SrcLoc
stackFrame =
  case Stack.getCallStack Stack.callStack of
    [] -> Nothing
    (_, srcLoc') : _ -> Just srcLoc'

data Service m = Service
  { endpoints :: Map.Map Text (Endpoint m),
    customTypes :: [Flat.CustomType]
  }

convert :: (forall a. m a -> n a) -> Service m -> Service n
convert nt service' =
  service'
    { endpoints =
        fmap
          (\(Endpoint name srcLoc f) -> Endpoint name srcLoc (nt . f))
          (endpoints service')
    }

data NamingCollision
  = DuplicateType Wire.TypeDefinition Wire.TypeDefinition
  | DuplicateConstructor Text Wire.TypeDefinition Wire.TypeDefinition
  | DuplicateEndpointName Text (Maybe Stack.SrcLoc) (Maybe Stack.SrcLoc)
  deriving (Show)

service :: [Endpoint m] -> Either NamingCollision (Service m)
service endpoints' = do
  customTypesAndDefs <-
    endpoints'
      & concatMap endpointTypes
      & findCustomTypes
  endpoints <-
    endpoints'
      & groupWith name
      & traverse
        ( \case
            endpoint' :| [] -> Right (name endpoint', endpoint')
            endpoint1 :| endpoint2 : _ ->
              Left
                ( DuplicateEndpointName
                    (name endpoint1)
                    (srcLoc endpoint1)
                    (srcLoc endpoint2)
                )
        )
      & fmap Map.fromList
  customTypesAndDefs
    & concatMap
      ( \(typeDef, customType) ->
          case Flat.subTypes customType of
            Left _ -> []
            Right constructors ->
              fmap
                (\constructor -> (constructor, typeDef))
                constructors
      )
    & groupWith (Flat.constructorName . fst)
    & traverse_
      ( \case
          _ :| [] -> Right ()
          (constructor, def1) :| (_, def2) : _ ->
            Left
              ( DuplicateConstructor
                  (Flat.constructorName constructor)
                  def1
                  def2
              )
      )
  Right Service {endpoints, customTypes = fmap snd customTypesAndDefs}

findCustomTypes :: [Wire.WireType] -> Either NamingCollision [(Wire.TypeDefinition, Flat.CustomType)]
findCustomTypes wireTypes =
  let typesByDef = foldr Flat.customTypesByDef Map.empty wireTypes
   in typesByDef
        & Map.toList
        & groupWith (Flat.typeName . snd)
        & traverse
          ( \case
              customType :| [] -> Right customType
              (def1, _) :| (def2, _) : _ -> Left (DuplicateType def1 def2)
          )

endpointTypes :: Endpoint m -> [Wire.WireType]
endpointTypes Endpoint {fn = (_ :: req -> m res)} =
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
    Just Endpoint {fn} -> do
      case Aeson.parseEither Wire.decode payload of
        Left parseErr -> handleErr (FailedToParseRequest (T.pack parseErr))
        Right req -> Encoding.encodingToLazyByteString . Wire.encode <$> fn req

wai :: Service IO -> Wai.Application
wai service' =
  \req respond -> do
    reqBytes <- Wai.strictRequestBody req
    res <- run service' reqBytes Control.Exception.throwIO
    respond (Wai.responseLBS (toEnum 200) [] res)
