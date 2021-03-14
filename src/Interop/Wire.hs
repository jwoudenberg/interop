{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
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
import Data.Kind (Constraint, Type)
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
  type HasKindOfType a = KindOfType (WhenStuckF (TypeError NoGenericInstanceError) (Rep a))
  rec :: Proxy a -> WireRec a
  default rec ::
    ( Generic a,
      WireG (HasKindOfType a) (Rep a)
    ) =>
    Proxy a ->
    WireRec a
  rec _ =
    WireRec
      { typeRec = typeG (Proxy :: Proxy (HasKindOfType a)) (Proxy :: Proxy (Rep a)),
        encodeRec = encodeG (Proxy :: Proxy (HasKindOfType a)) . from,
        decodeRec = fmap to . decodeG (Proxy :: Proxy (HasKindOfType a))
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
  type HasKindOfType Text = CustomType
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
  KindOfType (D1 ('MetaData name m p f) a) = KindOfConstructor name a

type family KindOfConstructor (typename :: Symbol) t where
  KindOfConstructor typename (C1 ('MetaCons n f b) U1) = RecordType
  KindOfConstructor typename (C1 ('MetaCons n f 'True) a) = Seq (FieldsHaveWireTypes a) RecordType
  KindOfConstructor typename V1 = TypeError AtLeastOneConstructorError
  KindOfConstructor typename (C1 ('MetaCons constructorname f 'False) (a :*: b)) =
    TypeError
      ( MustUseRecordNotationError
          typename
          constructorname
          (ParamTypes (a :*: b) '[])
      )
  KindOfConstructor typename (C1 ('MetaCons n f 'False) (S1 m (Rec0 a))) =
    Seq
      (EnsureRecord (WhenStuck (TypeError ParameterMustBeWireTypeError) (HasKindOfType a)))
      CustomType
  KindOfConstructor typename (a :+: b) = Seq (KindOfConstructor typename a) (Seq (KindOfConstructor typename b) CustomType)

type family EnsureRecord t where
  EnsureRecord RecordType = ()
  EnsureRecord t = TypeError ParameterMustBeRecordError

type family FieldsHaveWireTypes (t :: Type -> Type) :: Type where
  FieldsHaveWireTypes (a :*: b) = Seq (FieldsHaveWireTypes a) (FieldsHaveWireTypes b)
  FieldsHaveWireTypes (S1 m (Rec0 a)) = Seq (WhenStuck (TypeError FieldMustBeWireTypeError) (HasKindOfType a)) ()

type family ParamTypes (t :: Type -> Type) (acc :: [Type]) :: [Type] where
  ParamTypes (a :*: b) acc = ParamTypes a (ParamTypes b acc)
  ParamTypes (S1 m (Rec0 t)) acc = t ': acc

-- | Force evaluation of the first parameter, then return the second.
--
-- Learn more here: https://blog.csongor.co.uk/report-stuck-families/
type family Seq (a :: Type) (b :: k) :: k where
  Seq DoNotUse _ = Any
  Seq _ k = k

-- | Apply a constraint (custom type error) when the result of a type family is
-- stuck, that means has no result.
--
-- Example of a stuck type family computation:
--
-- > type StuckExample = HasKindOfType DoNotUse
--
-- This example is stuck because the `DoNotUse` type has no `Wire` instance, and
-- so doesn't have an associated type `HasKindOfType` either.
--
-- Learn more here: https://blog.csongor.co.uk/report-stuck-families/
type family WhenStuck (err :: Constraint) (a :: Type) :: Type where
  WhenStuck _ DoNotUse = Any
  WhenStuck _ k = k

type family WhenStuckF (err :: Constraint) (a :: Type -> Type) :: (Type -> Type) where
  WhenStuckF _ DoNotUseF = Any
  WhenStuckF _ k = k

-- Some unique types we require for the `Seq` and `WhenStuck` type families.
-- Do not use anywhere else or `Seq` and `WhenStuck` might not function.
data DoNotUse

data DoNotUseF a

type family Any :: k

type AtLeastOneConstructorError =
  'GHC.TypeLits.Text "Type must have at least one constructor to have a 'Wire' instance."

type MustUseRecordNotationError (typename :: Symbol) (constructorname :: Symbol) (params :: [Type]) =
  "Constructors with parameters need to use record syntax to have a 'Wire' instance."
    % "This will allow you to add and change fields in backwards-compatible ways in the future."
    % "Instead of:"
    % Indent ("data " <> typename <> " = " <> constructorname <> " " <> PrintParams params)
    % "Try:"
    % Indent ("data " <> typename <> " = " <> constructorname <> " " <> PrintParamsAsFields params)

type family PrintParams (params :: [Type]) :: GHC.TypeLits.ErrorMessage where
  PrintParams '[a] = ToErrorMessage a
  PrintParams '[a, b] = a <> " " <> b
  PrintParams (a ': b ': rest) = a <> " " <> b <> " ..."

type family PrintParamsAsFields (params :: [Type]) :: GHC.TypeLits.ErrorMessage where
  PrintParamsAsFields '[a] = "{ x :: " <> ToErrorMessage a <> " }"
  PrintParamsAsFields '[a, b] = "{ x :: " <> a <> ", y :: " <> b <> " }"
  PrintParamsAsFields (a ': b ': rest) = "{ x :: " <> a <> ", y :: " <> b <> ", ... }"

type ParameterMustBeRecordError =
  'GHC.TypeLits.Text "Constructor parameter must be a record."

type FieldMustBeWireTypeError =
  'GHC.TypeLits.Text "All the field types of a record with a Wire instance must themselves have a Wire instance."

type ParameterMustBeWireTypeError =
  'GHC.TypeLits.Text "Constructor parameters of a type with a Wire instance must themselves have a Wire instance"

type NoGenericInstanceError =
  'GHC.TypeLits.Text "Missing Generic instance."

data RecordType

data CustomType

-- | Some helpers for constructing nice type errors. API inspired by the
-- pretty-type-errors package, which seemed so easy to replicate that I prefered
-- doing that over taking a dependency.
--
-- https://hackage.haskell.org/package/type-errors-pretty
type family (a :: k) <> (b :: l) :: GHC.TypeLits.ErrorMessage where
  a <> b = ToErrorMessage a ':<>: ToErrorMessage b

type family (a :: k) % (b :: l) :: GHC.TypeLits.ErrorMessage where
  a % b = ToErrorMessage a ':$$: ToErrorMessage b

infixl 5 %

infixl 6 <>

type family ToErrorMessage (a :: k) :: GHC.TypeLits.ErrorMessage where
  ToErrorMessage (s :: Symbol) = 'GHC.TypeLits.Text s
  ToErrorMessage (e :: GHC.TypeLits.ErrorMessage) = e
  ToErrorMessage t = 'GHC.TypeLits.ShowType t

type family Indent (a :: GHC.TypeLits.ErrorMessage) :: GHC.TypeLits.ErrorMessage where
  Indent (a ':$$: b) = Indent a ':$$: Indent b
  Indent (a ':<>: b) = Indent a ':<>: b
  Indent ('GHC.TypeLits.ShowType a) = 'GHC.TypeLits.Text "    " ':<>: 'GHC.TypeLits.ShowType a
  Indent ('GHC.TypeLits.Text a) = 'GHC.TypeLits.Text "    " ':<>: 'GHC.TypeLits.Text a
