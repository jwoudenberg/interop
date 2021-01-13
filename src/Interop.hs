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
{-# LANGUAGE UndecidableInstances #-}

module Interop
  ( Endpoint,
    Service,
  )
where

import Control.Applicative ((<|>))
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
import GHC.TypeLits

data Endpoint m where
  Endpoint :: (Wire req, Wire res) => (req -> m res) -> Endpoint m

type Service m = [Endpoint m]

data WireType
  = Sum Text [(Text, WireType)]
  | Product [(Text, WireType)]
  | List WireType
  | Tuple [WireType]
  | Text
  | Int
  | Float
  | Bool
  | Unit

-- | Class representing types that can be encoded/decoded to wire format.
class Wire a where
  type_ :: Proxy a -> WireType
  encode :: a -> Aeson.Encoding
  decode :: Aeson.Value -> Aeson.Parser a
  default type_ :: WireG (Rep a) => Proxy a -> WireType
  type_ (_ :: Proxy a) = typeG (Proxy :: Proxy (Rep a))
  default encode :: (Generic a, WireG (Rep a)) => a -> Aeson.Encoding
  encode = encodeG . from
  default decode :: (Generic a, WireG (Rep a)) => Aeson.Value -> Aeson.Parser a
  decode = fmap to . decodeG

instance Wire Int where
  type_ _ = Int
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
  encodeG :: f a -> Aeson.Encoding
  decodeG :: Aeson.Value -> Aeson.Parser (f a)

instance
  ( KnownSymbol typename,
    CtorsG ctors
  ) =>
  WireG (D1 ('MetaData typename modname packname isnewtype) ctors)
  where
  typeG _ =
    Sum
      (T.pack (symbolVal (Proxy :: Proxy typename)))
      (typeCtorsG (Proxy :: Proxy ctors))
  encodeG = Aeson.pairs . encodeCtorsG . unM1
  decodeG = fmap M1 . Aeson.withObject (symbolVal (Proxy :: Proxy typename)) decodeCtorsG

class CtorsG f where
  typeCtorsG :: Proxy f -> [(Text, WireType)]
  encodeCtorsG :: f a -> Aeson.Series
  decodeCtorsG :: Aeson.Object -> Aeson.Parser (f a)

-- Instance for a constructor without any parameters:
--
-- > data Color
-- >    = Red
-- >    ...
--
instance
  ( KnownSymbol ctorname
  ) =>
  CtorsG (C1 ('MetaCons ctorname fix 'False) U1)
  where
  typeCtorsG _ =
    [ ( T.pack (symbolVal (Proxy :: Proxy ctorname)),
        Unit
      )
    ]
  encodeCtorsG (M1 U1) =
    Encoding.pair
      (T.pack (symbolVal (Proxy :: Proxy ctorname)))
      Encoding.null_
  decodeCtorsG obj =
    Aeson.explicitParseField
      (\_ -> pure (M1 U1))
      obj
      (T.pack (symbolVal (Proxy :: Proxy ctorname)))

-- Instance for a constructor with a single parameter:
--
-- > data Property
-- >    = Age Int
-- >    ...
--
instance
  ( KnownSymbol ctorname,
    Wire a
  ) =>
  CtorsG
    ( C1 ('MetaCons ctorname fix 'False)
        (S1 m (Rec0 a))
    )
  where
  typeCtorsG _ =
    [ ( T.pack (symbolVal (Proxy :: Proxy ctorname)),
        type_ (Proxy :: Proxy a)
      )
    ]
  encodeCtorsG (M1 (M1 (K1 x))) =
    Encoding.pair
      (T.pack (symbolVal (Proxy :: Proxy ctorname)))
      (encode x)
  decodeCtorsG obj =
    Aeson.explicitParseField
      decode
      obj
      (T.pack (symbolVal (Proxy :: Proxy ctorname)))
      & fmap (M1 . M1 . K1)

-- Instance for a constructor containing record fields:
--
-- > data Person
-- >   = Person
-- >       { name :: Text,
-- >         age :: Int
-- >       }
--
instance
  ( KnownSymbol ctorname,
    FieldsG fields
  ) =>
  CtorsG (C1 ('MetaCons ctorname fix 'True) fields)
  where
  typeCtorsG _ =
    [ ( T.pack (symbolVal (Proxy :: Proxy ctorname)),
        Product (typeFieldsG (Proxy :: Proxy fields))
      )
    ]
  encodeCtorsG (M1 fields) =
    encodeFieldsG fields
      & Encoding.pairs
      & Encoding.pair (T.pack (symbolVal (Proxy :: Proxy ctorname)))
  decodeCtorsG obj =
    Aeson.explicitParseField
      (Aeson.withObject (symbolVal (Proxy :: Proxy ctorname)) decodeFieldsG)
      obj
      (T.pack (symbolVal (Proxy :: Proxy ctorname)))
      & fmap M1

-- Instance producing compiler error for types without any constructors.
--
-- > data Never
--
instance
  (TypeError (GHC.TypeLits.Text "Type must have at least one constructor to have a 'Wire' instance.")) =>
  CtorsG V1
  where
  typeCtorsG _ = error "unreachable"
  encodeCtorsG _ = error "unreachable"
  decodeCtorsG _ = error "unreachable"

-- Instance producing compiler error for contructors with multiple parameters
-- that aren't record fields.
--
-- > data Coords = Coords Int Int
--
instance
  ( TypeError
      ( GHC.TypeLits.Text "Constructors with multiple parameters need to use record syntax to have a 'Wire' instance."
          :$$: GHC.TypeLits.Text "This will allow you to add and change fields in backwards-compatible ways in the future."
          :$$: GHC.TypeLits.Text "Instead of:"
          :$$: GHC.TypeLits.Text "    data Coords = Coords Int Int"
          :$$: GHC.TypeLits.Text "Try:"
          :$$: GHC.TypeLits.Text "    data Coords = Coords { x :: Int, y :: Int }"
      )
  ) =>
  CtorsG (C1 ('MetaCons ctorname fix 'False) (left :*: right))
  where
  typeCtorsG _ = error "unreachable"
  encodeCtorsG _ = error "unreachable"
  decodeCtorsG _ = error "unreachable"

instance
  ( CtorsG left,
    CtorsG right
  ) =>
  CtorsG (left :+: right)
  where
  typeCtorsG _ = typeCtorsG (Proxy :: Proxy left) <> typeCtorsG (Proxy :: Proxy right)
  encodeCtorsG (L1 left) = encodeCtorsG left
  encodeCtorsG (R1 right) = encodeCtorsG right
  decodeCtorsG obj = fmap L1 (decodeCtorsG obj) <|> fmap R1 (decodeCtorsG obj)

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
