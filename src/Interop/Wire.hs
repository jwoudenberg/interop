{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module dedicated to types that can be serialized and deserialized to allow
-- them to be sent over the wire.
module Interop.Wire
  ( WireType (..),
    type_,
    encode,
    decode,
    Constructor (..),
    TypeDefinition (..),
    Field (..),
    Wire (..),
  )
where

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
  | Record [Field]
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
  type HasKindOfType a
  type HasKindOfType a = KindOfType (Rep a)
  rec :: Proxy a -> WireRec a
  default rec :: (Generic a, WireG (KindOfType (Rep a)) (Rep a)) => Proxy a -> WireRec a
  rec _ =
    WireRec
      { typeRec = typeG (Proxy :: Proxy (KindOfType (Rep a))) (Proxy :: Proxy (Rep a)),
        encodeRec = encodeG (Proxy :: Proxy (KindOfType (Rep a))) . from,
        decodeRec = fmap to . decodeG (Proxy :: Proxy (KindOfType (Rep a)))
      }

data WireRec a = WireRec
  { typeRec :: WireType,
    encodeRec :: a -> Aeson.Encoding,
    decodeRec :: Aeson.Value -> Aeson.Parser a
  }

type_ :: Wire a => Proxy a -> WireType
type_ = typeRec . rec

encode :: Wire a => a -> Aeson.Encoding
encode = encodeRec (rec (Proxy :: Proxy a))

decode :: Wire a => Aeson.Value -> Aeson.Parser a
decode = decodeRec (rec (Proxy :: Proxy a))

instance Wire Int where
  type HasKindOfType Int = CustomType
  rec _ =
    WireRec
      { typeRec = Int,
        encodeRec = Encoding.int,
        decodeRec =
          Aeson.withScientific
            "Int"
            ( \scientific ->
                case toBoundedInteger scientific of
                  Nothing -> Aeson.unexpected "Expected an integer but got a float"
                  Just int -> pure int
            )
      }

instance Wire Data.Int.Int8 where
  type HasKindOfType Data.Int.Int8 = CustomType
  rec _ =
    WireRec
      { typeRec = Int,
        encodeRec = Encoding.int8,
        decodeRec =
          Aeson.withScientific
            "Int8"
            ( \scientific ->
                case toBoundedInteger scientific of
                  Nothing -> Aeson.unexpected "Expected an integer but got a float"
                  Just int -> pure int
            )
      }

instance Wire Data.Int.Int16 where
  rec _ =
    WireRec
      { typeRec = Int,
        encodeRec = Encoding.int16,
        decodeRec =
          Aeson.withScientific
            "Int16"
            ( \scientific ->
                case toBoundedInteger scientific of
                  Nothing -> Aeson.unexpected "Expected an integer but got a float"
                  Just int -> pure int
            )
      }

instance Wire Data.Int.Int32 where
  type HasKindOfType Data.Int.Int32 = CustomType
  rec _ =
    WireRec
      { typeRec = Int,
        encodeRec = Encoding.int32,
        decodeRec =
          Aeson.withScientific
            "Int32"
            ( \scientific ->
                case toBoundedInteger scientific of
                  Nothing -> Aeson.unexpected "Expected an integer but got a float"
                  Just int -> pure int
            )
      }

instance Wire Data.Int.Int64 where
  rec _ =
    WireRec
      { typeRec = Int,
        encodeRec = Encoding.int64,
        decodeRec =
          Aeson.withScientific
            "Int64"
            ( \scientific ->
                case toBoundedInteger scientific of
                  Nothing -> Aeson.unexpected "Expected an integer but got a float"
                  Just int -> pure int
            )
      }

instance Wire Word where
  type HasKindOfType Word = CustomType
  rec _ =
    WireRec
      { typeRec = Int,
        encodeRec = Encoding.word,
        decodeRec =
          Aeson.withScientific
            "Word"
            ( \scientific ->
                case toBoundedInteger scientific of
                  Nothing -> Aeson.unexpected "Expected an integer but got a float"
                  Just int -> pure int
            )
      }

instance Wire Data.Word.Word8 where
  rec _ =
    WireRec
      { typeRec = Int,
        encodeRec = Encoding.word8,
        decodeRec =
          Aeson.withScientific
            "Word8"
            ( \scientific ->
                case toBoundedInteger scientific of
                  Nothing -> Aeson.unexpected "Expected an integer but got a float"
                  Just int -> pure int
            )
      }

instance Wire Data.Word.Word16 where
  type HasKindOfType Data.Word.Word16 = CustomType
  rec _ =
    WireRec
      { typeRec = Int,
        encodeRec = Encoding.word16,
        decodeRec =
          Aeson.withScientific
            "Word16"
            ( \scientific ->
                case toBoundedInteger scientific of
                  Nothing -> Aeson.unexpected "Expected an integer but got a float"
                  Just int -> pure int
            )
      }

instance Wire Data.Word.Word32 where
  rec _ =
    WireRec
      { typeRec = Int,
        encodeRec = Encoding.word32,
        decodeRec =
          Aeson.withScientific
            "Word32"
            ( \scientific ->
                case toBoundedInteger scientific of
                  Nothing -> Aeson.unexpected "Expected an integer but got a float"
                  Just int -> pure int
            )
      }

instance Wire Data.Word.Word64 where
  type HasKindOfType Data.Word.Word64 = CustomType
  rec _ =
    WireRec
      { typeRec = Int,
        encodeRec = Encoding.word64,
        decodeRec =
          Aeson.withScientific
            "Word64"
            ( \scientific ->
                case toBoundedInteger scientific of
                  Nothing -> Aeson.unexpected "Expected an integer but got a float"
                  Just int -> pure int
            )
      }

instance Wire Float where
  rec _ =
    WireRec
      { typeRec = Float,
        encodeRec = Encoding.float,
        decodeRec = Aeson.withScientific "Float" (pure . toRealFloat)
      }

instance Wire Double where
  type HasKindOfType Double = CustomType
  rec _ =
    WireRec
      { typeRec = Float,
        encodeRec = Encoding.double,
        decodeRec = Aeson.withScientific "Double" (pure . toRealFloat)
      }

instance Wire Text where
  rec _ =
    WireRec
      { typeRec = Text,
        encodeRec = Encoding.text,
        decodeRec = Aeson.withText "Text" pure
      }

instance Wire Bool where
  type HasKindOfType Bool = CustomType
  rec _ =
    WireRec
      { typeRec = Bool,
        encodeRec = Encoding.bool,
        decodeRec = Aeson.withBool "Bool" pure
      }

instance Wire a => Wire (Maybe a) where
  type HasKindOfType (Maybe a) = CustomType
  rec _ =
    WireRec
      { typeRec = Optional (type_ (Proxy :: Proxy a)),
        encodeRec = maybe Encoding.null_ encode,
        decodeRec = \val -> (Just <$> decode val) <|> pure Nothing
      }

instance Wire a => Wire [a] where
  type HasKindOfType [a] = CustomType
  rec _ =
    WireRec
      { typeRec = List (type_ (Proxy :: Proxy a)),
        encodeRec = Encoding.list encode,
        decodeRec = Aeson.withArray "[]" (traverse decode . Foldable.toList)
      }

instance Wire () where
  type HasKindOfType () = CustomType
  rec _ =
    WireRec
      { typeRec = Unit,
        encodeRec = \_ -> Encoding.emptyArray_,
        decodeRec = \_ -> pure ()
      }

class WireG kindOfType f where
  typeG :: Proxy kindOfType -> Proxy f -> WireType
  encodeG :: Proxy kindOfType -> f a -> Aeson.Encoding
  decodeG :: Proxy kindOfType -> Aeson.Value -> Aeson.Parser (f a)

instance
  ( KnownSymbol typename,
    KnownSymbol packname,
    KnownSymbol modname,
    CtorsG ctors
  ) =>
  WireG CustomType (D1 ('MetaData typename modname packname isnewtype) ctors)
  where
  typeG _ _ =
    Type
      TypeDefinition
        { packageName = T.pack (symbolVal (Proxy :: Proxy packname)),
          moduleName = T.pack (symbolVal (Proxy :: Proxy modname)),
          typeName = T.pack (symbolVal (Proxy :: Proxy typename))
        }
      (typeCtorsG (Proxy :: Proxy ctors))
  encodeG _ = Aeson.pairs . encodeCtorsG . unM1
  decodeG _ = fmap M1 . Aeson.withObject (symbolVal (Proxy :: Proxy typename)) decodeCtorsG

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
      Encoding.emptyObject_
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
  WireG RecordType (D1 metadata (C1 ('MetaCons ctorname fix 'True) fields))
  where
  typeG _ _ = Record (typeFieldsG (Proxy :: Proxy fields))
  encodeG _ = Encoding.pairs . encodeFieldsG . unM1 . unM1
  decodeG _ = fmap (M1 . M1) . Aeson.withObject (symbolVal (Proxy :: Proxy ctorname)) decodeFieldsG

instance
  ( Generic sub,
    FieldsG (Rep sub),
    KnownSymbol ctorname
  ) =>
  CtorsG (C1 ('MetaCons ctorname fix 'False) (S1 metasel (Rec0 sub)))
  where
  typeCtorsG _ =
    [ Constructor
        (T.pack (symbolVal (Proxy :: Proxy ctorname)))
        (typeFieldsG (Proxy :: Proxy (Rep sub)))
    ]
  encodeCtorsG (M1 (M1 (K1 x))) =
    from x
      & encodeFieldsG
      & Aeson.pairs
      & Encoding.pair (T.pack (symbolVal (Proxy :: Proxy ctorname)))
  decodeCtorsG obj =
    Aeson.explicitParseField
      (Aeson.withObject (symbolVal (Proxy :: Proxy ctorname)) decodeFieldsG)
      obj
      (T.pack (symbolVal (Proxy :: Proxy ctorname)))
      & fmap (M1 . M1 . K1 . to)

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
  (FieldsG fields) =>
  FieldsG (D1 metadata (C1 metacons fields))
  where
  typeFieldsG _ = typeFieldsG (Proxy :: Proxy fields)
  encodeFieldsG = encodeFieldsG . unM1 . unM1
  decodeFieldsG = fmap (M1 . M1) . decodeFieldsG

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

-- | Depending on the kind of type we're dealing with (record, multiple
-- constructors, ...) we want a different set of Generics instances deriving the
-- Wire instances. This would result in OverlappingInstance compilation errors.
--
-- To avoid these we disambiguate our instances by adding an additional type
-- class parameter 'kindOfType' that will be different for each kind of type we
-- define a Wire instance for.
--
-- This type family returns the kindOfType belonging to a particular generics
-- representation.
type family KindOfType t where
  KindOfType (D1 m a) = KindOfType a
  KindOfType (C1 ('MetaCons n f 'True) a) = RecordType
  KindOfType (C1 ('MetaCons n f b) U1) = RecordType
  KindOfType V1 = TypeError AtLeastOneConstructorError
  KindOfType (C1 ('MetaCons n f 'False) (a :*: b)) = TypeError MustUseRecordNotationError
  KindOfType (C1 ('MetaCons n f 'False) (S1 m (Rec0 a))) = Seq (EnsureRecord (HasKindOfType a)) CustomType
  KindOfType (a :+: b) = Seq (KindOfType a) (Seq (KindOfType b) CustomType)

type family EnsureRecord t where
  EnsureRecord RecordType = ()
  EnsureRecord t = TypeError ParameterMustBeRecordError

-- | Force evaluation of the first parameter, then return the second.
-- Learn more here: https://blog.csongor.co.uk/report-stuck-families/
type family Seq a b where
  Seq DoNotUse b = DoNotUse
  Seq a b = b

data DoNotUse

type AtLeastOneConstructorError =
  'GHC.TypeLits.Text "Type must have at least one constructor to have a 'Wire' instance."

type MustUseRecordNotationError =
  'GHC.TypeLits.Text "Constructors with parameters need to use record syntax to have a 'Wire' instance."
    ':$$: 'GHC.TypeLits.Text "This will allow you to add and change fields in backwards-compatible ways in the future."
    ':$$: 'GHC.TypeLits.Text "Instead of:"
    ':$$: 'GHC.TypeLits.Text "    data Coords = Coords Int Int"
    ':$$: 'GHC.TypeLits.Text "Try:"
    ':$$: 'GHC.TypeLits.Text "    data Coords = Coords { x :: Int, y :: Int }"

type ParameterMustBeRecordError =
  'GHC.TypeLits.Text "Constructor parameter must be a record."

data RecordType

data CustomType
