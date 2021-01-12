{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Interop
  ( Endpoint,
    Service,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)

data Endpoint m where
  Endpoint :: (Wire req, Wire res) => (req -> m res) -> Endpoint m

type Service m = [Endpoint m]

data WireType
  = Product [(Text, WireType)]
  | Sum [(Text, WireType)]
  | List WireType
  | Text
  | Int
  | Float
  | Bool

-- | Class representing types that can be encoded/decoded to wire format.
class Wire a where
  type_ :: Proxy a -> WireType
  name :: Proxy a -> Text
  encode :: a -> Aeson.Encoding
  decode :: Aeson.Value -> Aeson.Parser a
  default type_ :: WireG (Rep a) => Proxy a -> WireType
  type_ (_ :: Proxy a) = typeG (Proxy :: Proxy (Rep a))
  default name :: WireG (Rep a) => Proxy a -> Text
  name (_ :: Proxy a) = nameG (Proxy :: Proxy (Rep a))
  default encode :: (Generic a, WireG (Rep a)) => a -> Aeson.Encoding
  encode = encodeG . from
  default decode :: (Generic a, WireG (Rep a)) => Aeson.Value -> Aeson.Parser a
  decode = fmap to . decodeG

instance Wire Int where
  type_ _ = Int
  name _ = "Int"
  encode = Encoding.int
  decode =
    Aeson.withScientific
      "Int"
      ( \scientific ->
          case toBoundedInteger scientific of
            Nothing -> Aeson.unexpected "Expected an integer but got a float"
            Just int -> pure int
      )

class WireG f where
  typeG :: Proxy f -> WireType
  nameG :: Proxy f -> Text
  encodeG :: f a -> Aeson.Encoding
  decodeG :: Aeson.Value -> Aeson.Parser (f a)

instance
  ( KnownSymbol typename,
    FieldsG fields
  ) =>
  WireG
    ( D1 ('MetaData typename modname packname isnewtype)
        (C1 ('MetaCons ctorname fix 'True) fields)
    )
  where
  typeG _ = Product (typeFieldsG (Proxy :: Proxy fields))
  nameG _ = T.pack (symbolVal (Proxy :: Proxy typename))
  encodeG = Encoding.pairs . encodeFieldsG . unM1 . unM1
  decodeG = fmap (M1 . M1) . Aeson.withObject (symbolVal (Proxy :: Proxy typename)) decodeFieldsG

class FieldsG f where
  typeFieldsG :: Proxy f -> [(Text, WireType)]
  encodeFieldsG :: f a -> Aeson.Series
  decodeFieldsG :: Aeson.Object -> Aeson.Parser (f a)

instance
  ( KnownSymbol fieldname,
    Wire field
  ) =>
  FieldsG (S1 ('MetaSel ('Just fieldname) unpackedness strictness lazyness) (Rec0 field))
  where
  typeFieldsG _ =
    [ ( T.pack (symbolVal (Proxy :: Proxy fieldname)),
        type_ (Proxy :: Proxy field)
      )
    ]
  encodeFieldsG = Encoding.pair (T.pack (symbolVal (Proxy :: Proxy fieldname))) . encode . unK1 . unM1
  decodeFieldsG obj = fmap (M1 . K1) $ Aeson.explicitParseField decode obj (T.pack (symbolVal (Proxy :: Proxy fieldname)))

instance
  ( FieldsG left,
    FieldsG right
  ) =>
  FieldsG (left :*: right)
  where
  typeFieldsG _ = typeFieldsG (Proxy :: Proxy left) <> typeFieldsG (Proxy :: Proxy right)
  encodeFieldsG (left :*: right) = encodeFieldsG left <> encodeFieldsG right
  decodeFieldsG obj = (:*:) <$> decodeFieldsG obj <*> decodeFieldsG obj

respond :: Functor m => Service m -> ByteString -> Either Text (m ByteString)
respond endpoints request =
  endpoints
    & fmap (respondEndpoint request)
    & foldl' (<>) (Left "API has no endpoints for handling requests")

respondEndpoint :: Functor m => ByteString -> Endpoint m -> Either Text (m ByteString)
respondEndpoint req (Endpoint f) = do
  value <- first T.pack (Aeson.eitherDecode req)
  x <- first T.pack (Aeson.parseEither decode value)
  pure $ Encoding.encodingToLazyByteString . encode <$> f x

type Port = Int

serveHttp :: Port -> Service IO -> IO ()
serveHttp = undefined

generateRubyClient :: FilePath -> Service m -> IO ()
generateRubyClient = undefined

saveSpec :: FilePath -> Service m -> IO ()
saveSpec = undefined

loadSpec :: FilePath -> IO (Service Proxy)
loadSpec = undefined

-- | Lists breaking and non-breaking changes in the API.
changelog :: Service m -> Service n -> Text
changelog = undefined
