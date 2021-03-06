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

module Interop.Generics where

import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.Types as Aeson
import qualified Data.Foldable as Foldable
import Data.Function ((&))
import qualified Data.Int
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (toBoundedInteger, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Word
import GHC.Generics hiding (Constructor, moduleName, packageName)
import GHC.TypeLits hiding (Text)
import qualified GHC.TypeLits

data WireType
  = Type TypeDefinition [Constructor]
  | List WireType
  | Optional WireType
  | Unit
  | Text
  | Int
  | Float
  | Bool
  deriving (Show)

data Constructor = Constructor
  { constructorName :: Text,
    fields :: [Field]
  }
  deriving (Show)

data Field = Field
  { fieldName :: Text,
    fieldType :: WireType
  }
  deriving (Show)

data TypeDefinition = TypeDefinition
  { packageName :: Text,
    moduleName :: Text,
    typeName :: Text
  }
  deriving (Eq, Ord, Show)

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

instance Wire Data.Int.Int8 where
  type_ _ = Int
  encode = Encoding.int8
  decode =
    Aeson.withScientific
      "Int8"
      ( \scientific ->
          case toBoundedInteger scientific of
            Nothing -> Aeson.unexpected "Expected an integer but got a float"
            Just int -> pure int
      )

instance Wire Data.Int.Int16 where
  type_ _ = Int
  encode = Encoding.int16
  decode =
    Aeson.withScientific
      "Int16"
      ( \scientific ->
          case toBoundedInteger scientific of
            Nothing -> Aeson.unexpected "Expected an integer but got a float"
            Just int -> pure int
      )

instance Wire Data.Int.Int32 where
  type_ _ = Int
  encode = Encoding.int32
  decode =
    Aeson.withScientific
      "Int32"
      ( \scientific ->
          case toBoundedInteger scientific of
            Nothing -> Aeson.unexpected "Expected an integer but got a float"
            Just int -> pure int
      )

instance Wire Data.Int.Int64 where
  type_ _ = Int
  encode = Encoding.int64
  decode =
    Aeson.withScientific
      "Int64"
      ( \scientific ->
          case toBoundedInteger scientific of
            Nothing -> Aeson.unexpected "Expected an integer but got a float"
            Just int -> pure int
      )

instance Wire Word where
  type_ _ = Int
  encode = Encoding.word
  decode =
    Aeson.withScientific
      "Word"
      ( \scientific ->
          case toBoundedInteger scientific of
            Nothing -> Aeson.unexpected "Expected an integer but got a float"
            Just int -> pure int
      )

instance Wire Data.Word.Word8 where
  type_ _ = Int
  encode = Encoding.word8
  decode =
    Aeson.withScientific
      "Word8"
      ( \scientific ->
          case toBoundedInteger scientific of
            Nothing -> Aeson.unexpected "Expected an integer but got a float"
            Just int -> pure int
      )

instance Wire Data.Word.Word16 where
  type_ _ = Int
  encode = Encoding.word16
  decode =
    Aeson.withScientific
      "Word16"
      ( \scientific ->
          case toBoundedInteger scientific of
            Nothing -> Aeson.unexpected "Expected an integer but got a float"
            Just int -> pure int
      )

instance Wire Data.Word.Word32 where
  type_ _ = Int
  encode = Encoding.word32
  decode =
    Aeson.withScientific
      "Word32"
      ( \scientific ->
          case toBoundedInteger scientific of
            Nothing -> Aeson.unexpected "Expected an integer but got a float"
            Just int -> pure int
      )

instance Wire Data.Word.Word64 where
  type_ _ = Int
  encode = Encoding.word64
  decode =
    Aeson.withScientific
      "Word64"
      ( \scientific ->
          case toBoundedInteger scientific of
            Nothing -> Aeson.unexpected "Expected an integer but got a float"
            Just int -> pure int
      )

instance Wire Float where
  type_ _ = Float
  encode = Encoding.float
  decode = Aeson.withScientific "Float" (pure . toRealFloat)

instance Wire Double where
  type_ _ = Float
  encode = Encoding.double
  decode = Aeson.withScientific "Double" (pure . toRealFloat)

instance Wire Text where
  type_ _ = Text
  encode = Encoding.text
  decode = Aeson.withText "Text" pure

instance Wire String where
  type_ _ = Text
  encode = Encoding.string
  decode = Aeson.withText "String" (pure . T.unpack)

instance Wire Bool where
  type_ _ = Bool
  encode = Encoding.bool
  decode = Aeson.withBool "Bool" pure

instance Wire a => Wire (Maybe a) where
  type_ _ = Optional (type_ (Proxy :: Proxy a))
  encode = maybe Encoding.null_ encode
  decode val = (Just <$> decode val) <|> pure Nothing

instance Wire a => Wire [a] where
  type_ _ = List (type_ (Proxy :: Proxy a))
  encode = Encoding.list encode
  decode = Aeson.withArray "[]" (traverse decode . Foldable.toList)

instance Wire () where
  type_ _ = Unit
  encode _ = Encoding.emptyArray_
  decode _ = pure ()

class WireG f where
  typeG :: Proxy f -> WireType
  encodeG :: f a -> Aeson.Encoding
  decodeG :: Aeson.Value -> Aeson.Parser (f a)

instance
  ( KnownSymbol typename,
    KnownSymbol packname,
    KnownSymbol modname,
    CtorsG ctors
  ) =>
  WireG (D1 ('MetaData typename modname packname isnewtype) ctors)
  where
  typeG _ =
    Type
      TypeDefinition
        { packageName = T.pack (symbolVal (Proxy :: Proxy packname)),
          moduleName = T.pack (symbolVal (Proxy :: Proxy modname)),
          typeName = T.pack (symbolVal (Proxy :: Proxy typename))
        }
      (typeCtorsG (Proxy :: Proxy ctors))
  encodeG = Aeson.pairs . encodeCtorsG . unM1
  decodeG = fmap M1 . Aeson.withObject (symbolVal (Proxy :: Proxy typename)) decodeCtorsG

class CtorsG f where
  typeCtorsG :: Proxy f -> [Constructor]
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
    [ Constructor
        (T.pack (symbolVal (Proxy :: Proxy ctorname)))
        []
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
    [ Constructor
        (T.pack (symbolVal (Proxy :: Proxy ctorname)))
        (typeFieldsG (Proxy :: Proxy fields))
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
  (TypeError ('GHC.TypeLits.Text "Type must have at least one constructor to have a 'Wire' instance.")) =>
  CtorsG V1
  where
  typeCtorsG _ = error "unreachable"
  encodeCtorsG _ = error "unreachable"
  decodeCtorsG _ = error "unreachable"

-- Instance producing compiler error for contructors with parameters that
-- aren't record fields.
--
-- > data Coords = Coords Int Int
--
instance
  ( TypeError
      ( 'GHC.TypeLits.Text "Constructors with parameters need to use record syntax to have a 'Wire' instance."
          ':$$: 'GHC.TypeLits.Text "This will allow you to add and change fields in backwards-compatible ways in the future."
          ':$$: 'GHC.TypeLits.Text "Instead of:"
          ':$$: 'GHC.TypeLits.Text "    data Coords = Coords Int Int"
          ':$$: 'GHC.TypeLits.Text "Try:"
          ':$$: 'GHC.TypeLits.Text "    data Coords = Coords { x :: Int, y :: Int }"
      )
  ) =>
  CtorsG (C1 ('MetaCons ctorname fix 'False) params)
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
  typeFieldsG :: Proxy f -> [Field]
  encodeFieldsG :: f a -> Aeson.Series
  decodeFieldsG :: Aeson.Object -> Aeson.Parser (f a)

instance
  ( KnownSymbol fieldname,
    ParseField (IsMaybe field) field,
    Wire field,
    Wire (Unwrapped (IsMaybe field) field)
  ) =>
  FieldsG (S1 ('MetaSel ('Just fieldname) unpackedness strictness lazyness) (Rec0 field))
  where
  typeFieldsG _ =
    [ Field
        (T.pack (symbolVal (Proxy :: Proxy fieldname)))
        (type_ (Proxy :: Proxy field))
    ]
  encodeFieldsG = Encoding.pair (T.pack (symbolVal (Proxy :: Proxy fieldname))) . encode . unK1 . unM1
  decodeFieldsG obj =
    parseField
      (Proxy :: Proxy (IsMaybe field))
      decode
      obj
      (T.pack (symbolVal (Proxy :: Proxy fieldname)))
      & fmap (M1 . K1)

instance
  ( FieldsG left,
    FieldsG right
  ) =>
  FieldsG (left :*: right)
  where
  typeFieldsG _ = typeFieldsG (Proxy :: Proxy left) <> typeFieldsG (Proxy :: Proxy right)
  encodeFieldsG (left :*: right) = encodeFieldsG left <> encodeFieldsG right
  decodeFieldsG obj = (:*:) <$> decodeFieldsG obj <*> decodeFieldsG obj

-- Aeson provides two functions for using a custom parser to decode the field
-- of an object: 'explicitParseField' and 'explicitParseFieldMaybe'. When the
-- type of the field is a 'Maybe a' we want to use the second function, because
-- it doesn't failed parsing a JSON object missing that field. When the type of
-- the field is not 'Maybe a' we have to use 'explicitParseField'.
--
-- This type class and its supporting type family 'IsMaybe' exist solely to pick
-- the right Aeson helper function depending on whether the type of the field is
-- a 'Maybe a' or not.
class ParseField (isMaybe :: Bool) a where
  type Unwrapped isMaybe a
  parseField ::
    Proxy isMaybe ->
    (Aeson.Value -> Aeson.Parser (Unwrapped isMaybe a)) ->
    Aeson.Object ->
    Text ->
    Aeson.Parser a

instance ParseField 'True (Maybe a) where
  type Unwrapped 'True (Maybe a) = a
  parseField _ = Aeson.explicitParseFieldMaybe

instance ParseField 'False a where
  type Unwrapped 'False a = a
  parseField _ = Aeson.explicitParseField

type family IsMaybe a :: Bool where
  IsMaybe (Maybe a) = 'True
  IsMaybe a = 'False
