{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Interop.Service
  ( Service (..),
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
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (traverse_)
import Data.Function ((&))
import qualified Data.List
import qualified Data.List.NonEmpty as NonEmpty
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

namingCollisionToText :: NamingCollision -> Text
namingCollisionToText namingCollision =
  case namingCollision of
    DuplicateType def1 def2 ->
      T.unlines
        [ "The service uses two types with the same name:",
          "",
          "  " <> Wire.moduleName def1 <> "." <> Wire.typeName def1,
          "  " <> Wire.moduleName def2 <> "." <> Wire.typeName def2,
          "",
          "Try renaming one of the types.",
          "",
          "Unique names avoid potential naming conflicts in generated client",
          "code. There's many tricks I could play to avoid conflicts in",
          "generated code, such as adding suffixes to dupplicate names or",
          "generating multiple files if necessary. All of these make it harder",
          "to understand how I work though, so I'd prefer not to."
        ]
    DuplicateConstructor name def1 def2 ->
      T.unlines
        [ "The service uses two constructors with the same name:",
          "",
          "  " <> Wire.moduleName def1 <> "." <> Wire.typeName def1 <> "." <> name,
          "  " <> Wire.moduleName def2 <> "." <> Wire.typeName def2 <> "." <> name,
          "",
          "Try renaming one of the constructors.",
          "",
          "Unique names avoid potential naming conflicts in generated client",
          "code. There's many tricks I could play to avoid conflicts in",
          "generated code, such as adding suffixes to dupplicate names or",
          "generating multiple files if necessary. All of these make it harder",
          "to understand how I work though, so I'd prefer not to."
        ]
    DuplicateEndpointName name loc1 loc2 ->
      T.unlines
        [ "This service contains two endpoints with the same name:",
          "",
          "  " <> name <> "   " <> maybe "" printSrcLoc loc1,
          "  " <> name <> "   " <> maybe "" printSrcLoc loc2,
          "",
          "Try renaming one of the constructors.",
          "",
          "If two endpoints have the same name then when I receive a request",
          "I won't know which endpoint should handle it."
        ]

printSrcLoc :: Stack.SrcLoc -> Text
printSrcLoc srcLoc =
  T.pack (Stack.srcLocFile srcLoc)
    <> ":"
    <> T.pack (show (Stack.srcLocStartLine srcLoc))

service :: [Endpoint m] -> Either Text (Service m)
service endpoints' = first namingCollisionToText $ do
  customTypesAndDefs <-
    endpoints'
      & concatMap endpointTypes
      & findCustomTypes
  endpoints <-
    endpoints'
      & groupWith name
      & traverse
        ( \case
            endpoint' NonEmpty.:| [] -> Right (name endpoint', endpoint')
            endpoint1 NonEmpty.:| endpoint2 : _ ->
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
          _ NonEmpty.:| [] -> Right ()
          (constructor, def1) NonEmpty.:| (_, def2) : _ ->
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
              customType NonEmpty.:| [] -> Right customType
              (def1, _) NonEmpty.:| (def2, _) : _ -> Left (DuplicateType def1 def2)
          )

endpointTypes :: Endpoint m -> [Wire.WireType]
endpointTypes Endpoint {fn = (_ :: req -> m res)} =
  [ Wire.type_ (Proxy :: Proxy req),
    Wire.type_ (Proxy :: Proxy res)
  ]

groupWith :: Ord b => (a -> b) -> [a] -> [NonEmpty.NonEmpty a]
groupWith select xs =
  Data.List.sortOn select xs
    & NonEmpty.groupWith select

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
