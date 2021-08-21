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
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import qualified Data.Int
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (toBoundedInteger, toRealFloat)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Word
import GHC.Generics hiding (Constructor, moduleName, packageName)
import GHC.TypeLits hiding (Text)
import qualified GHC.TypeLits

data WireType
  = Type TypeDefinition (Either [Field] [Constructor])
  | List WireType
  | Optional WireType
  | Dict WireType WireType
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
  type HasKindOfType a = KindOfType (WhenStuckF (TypeError (NoGenericInstanceError a)) (Rep a))
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
  type HasKindOfType Data.Int.Int16 = CustomType
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
  type HasKindOfType Data.Int.Int64 = CustomType
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
  type HasKindOfType Data.Word.Word8 = CustomType
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
  type HasKindOfType Data.Word.Word32 = CustomType
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
  type HasKindOfType Float = CustomType
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

instance (Ord k, Wire k, Wire v) => Wire (Map.Map k v) where
  type HasKindOfType (Map.Map k v) = CustomType
  rec _ =
    WireRec
      { typeRec = Dict (type_ (Proxy :: Proxy k)) (type_ (Proxy :: Proxy v)),
        encodeRec = Encoding.list (\(k, v) -> Encoding.list id [encode k, encode v]) . Map.toList,
        decodeRec =
          Aeson.withArray
            "[]"
            ( fmap Map.fromList
                . traverse
                  ( Aeson.withArray
                      "(k,v)"
                      ( \tuple ->
                          case Foldable.toList tuple of
                            [k, v] -> (,) <$> decode k <*> decode v
                            _ -> fail "Expected array to have exactly two elements."
                      )
                  )
                . Foldable.toList
            )
      }

instance (Eq k, Hashable k, Wire k, Wire v) => Wire (HashMap.HashMap k v) where
  type HasKindOfType (HashMap.HashMap k v) = CustomType
  rec _ =
    WireRec
      { typeRec = Dict (type_ (Proxy :: Proxy k)) (type_ (Proxy :: Proxy v)),
        encodeRec = Encoding.list (\(k, v) -> Encoding.list id [encode k, encode v]) . HashMap.toList,
        decodeRec =
          Aeson.withArray
            "[]"
            ( fmap HashMap.fromList
                . traverse
                  ( Aeson.withArray
                      "(k,v)"
                      ( \tuple ->
                          case Foldable.toList tuple of
                            [k, v] -> (,) <$> decode k <*> decode v
                            _ -> fail "Expected array to have exactly two elements."
                      )
                  )
                . Foldable.toList
            )
      }

instance (Ord a, Wire a) => Wire (Set.Set a) where
  type HasKindOfType (Set.Set a) = CustomType
  rec _ =
    WireRec
      { typeRec = List (type_ (Proxy :: Proxy a)),
        encodeRec = encode . Set.toList,
        decodeRec = fmap Set.fromList . decode
      }

instance (Eq a, Hashable a, Wire a) => Wire (HashSet.HashSet a) where
  type HasKindOfType (HashSet.HashSet a) = CustomType
  rec _ =
    WireRec
      { typeRec = List (type_ (Proxy :: Proxy a)),
        encodeRec = encode . HashSet.toList,
        decodeRec = fmap HashSet.fromList . decode
      }

instance (Wire a) => Wire (Seq.Seq a) where
  type HasKindOfType (Seq.Seq a) = CustomType
  rec _ =
    WireRec
      { typeRec = List (type_ (Proxy :: Proxy a)),
        encodeRec = encode . Foldable.toList,
        decodeRec = fmap Seq.fromList . decode
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
      (Right (typeCtorsG (Proxy :: Proxy ctors)))
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
  ( KnownSymbol typename,
    KnownSymbol packname,
    KnownSymbol modname,
    KnownSymbol ctorname,
    FieldsG fields
  ) =>
  WireG
    RecordType
    ( D1
        ('MetaData typename modname packname isnewtype)
        (C1 ('MetaCons ctorname fix isrecord) fields)
    )
  where
  typeG _ _ =
    Type
      TypeDefinition
        { packageName = T.pack (symbolVal (Proxy :: Proxy packname)),
          moduleName = T.pack (symbolVal (Proxy :: Proxy modname)),
          typeName = T.pack (symbolVal (Proxy :: Proxy typename))
        }
      (Left (typeFieldsG (Proxy :: Proxy fields)))
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
    ParseField (IsOptional field) field,
    Wire field,
    Wire (Unwrapped (IsOptional field) field)
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
      (Proxy :: Proxy (IsOptional field))
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

instance FieldsG U1 where
  typeFieldsG _ = []
  encodeFieldsG _ = mempty
  decodeFieldsG _ = pure U1

-- Aeson provides two functions for using a custom parser to decode the field
-- of an object: 'explicitParseField' and 'explicitParseFieldMaybe'. When the
-- type of the field is a 'Maybe a' we want to use the second function, because
-- it doesn't failed parsing a JSON object missing that field. When the type of
-- the field is not 'Maybe a' we have to use 'explicitParseField'.
--
-- This type class and its supporting type family 'IsOptional' exist solely to
-- pick the right Aeson helper function depending on whether the type of the
-- field is a 'Maybe a' or not.
--
-- Addendum: this has been extended to cover not just 'Maybe a', but other types
-- with an empty value like lists and dictionaries too. For these types, when a
-- null value is passed we want to gracefully recover by letting decoding return
-- an empty value.
class ParseField (isMaybe :: Bool) a where
  type Unwrapped isMaybe a
  type Unwrapped bool a = a
  parseField ::
    Proxy isMaybe ->
    (Aeson.Value -> Aeson.Parser (Unwrapped isMaybe a)) ->
    Aeson.Object ->
    Text ->
    Aeson.Parser a
  default parseField ::
    (Monoid a, Unwrapped isMaybe a ~ a) =>
    Proxy isMaybe ->
    (Aeson.Value -> Aeson.Parser (Unwrapped isMaybe a)) ->
    Aeson.Object ->
    Text ->
    Aeson.Parser a
  parseField _ parse object key =
    Aeson.explicitParseFieldMaybe parse object key
      & fmap (Data.Maybe.fromMaybe mempty)

instance ParseField 'True (Maybe a) where
  type Unwrapped 'True (Maybe a) = a
  parseField _ = Aeson.explicitParseFieldMaybe

instance ParseField 'True [a]

instance (Eq k, Hashable k) => ParseField 'True (HashMap.HashMap k v)

instance (Eq a, Hashable a) => ParseField 'True (HashSet.HashSet a)

instance Ord k => ParseField 'True (Map.Map k v)

instance Ord a => ParseField 'True (Set.Set a)

instance Ord a => ParseField 'True (Seq.Seq a)

instance ParseField 'False a where
  type Unwrapped 'False a = a
  parseField _ = Aeson.explicitParseField

type family IsOptional a :: Bool where
  IsOptional (Maybe a) = 'True
  IsOptional [a] = 'True
  IsOptional (HashMap.HashMap k v) = 'True
  IsOptional (HashSet.HashSet a) = 'True
  IsOptional (Map.Map k v) = 'True
  IsOptional (Set.Set a) = 'True
  IsOptional (Seq.Seq a) = 'True
  IsOptional a = 'False

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
  KindOfType (D1 ('MetaData typename m p f) V1) =
    TypeError (AtLeastOneConstructorError typename)
  KindOfType (D1 ('MetaData typename m p f) (a :+: b)) =
    Seq
      (ValidateConstructors typename 'False (Constructors (a :+: b)))
      CustomType
  KindOfType (D1 ('MetaData typename m p f) a) =
    KindOfConstructor typename a

type family KindOfConstructor (typename :: Symbol) (constructor :: Type -> Type) :: Type where
  KindOfConstructor typename (C1 ('MetaCons n f b) U1) =
    RecordType
  KindOfConstructor typename (C1 ('MetaCons constructorname f 'True) a) =
    Seq (FieldsHaveWireTypes typename constructorname 'OnlyElem a) RecordType
  KindOfConstructor typename (C1 ('MetaCons constructorname f 'False) (a :*: b)) =
    TypeError
      ( MustUseRecordNotationError
          typename
          constructorname
          (ParamTypes (a :*: b) '[])
      )
  KindOfConstructor typename (C1 ('MetaCons constructorname f 'False) (S1 ms (Rec0 a))) =
    Seq
      ( EnsureRecord
          typename
          constructorname
          '[a]
          (WhenStuck (TypeError (ParameterMustBeWireTypeError typename a)) (HasKindOfType a))
      )
      CustomType

type family
  ValidateConstructors
    (typename :: Symbol)
    (before :: Bool)
    (xs :: [Type -> Type]) ::
    Type
  where
  ValidateConstructors typename before '[] =
    ()
  ValidateConstructors typename before '[x] =
    ValidateSingleConstructor typename before x 'False
  ValidateConstructors typename before (x ': rest) =
    Seq
      (ValidateSingleConstructor typename before x 'True)
      (ValidateConstructors typename 'True rest)

type family
  ValidateSingleConstructor
    (typename :: Symbol)
    (before :: Bool)
    (x :: Type -> Type)
    (after :: Bool) ::
    Type
  where
  ValidateSingleConstructor typename before (C1 mc U1) after =
    ()
  ValidateSingleConstructor typename before (C1 ('MetaCons constructorname f 'False) (a :*: b)) after =
    TypeError
      ( MustUseRecordTypeInsteadOfParams
          typename
          before
          after
          constructorname
          (ParamTypes (a :*: b) '[])
      )
  ValidateSingleConstructor typename before (C1 ('MetaCons constructorname f 'True) params) after =
    TypeError (UseSeparateRecordType typename before after constructorname (FieldTypes params '[]))
  ValidateSingleConstructor typename before (C1 ('MetaCons constructorname f 'False) (S1 ms (Rec0 a))) after =
    Seq
      ( WhenStuck
          ( TypeError
              ( MustUseRecordTypeInsteadOfParams
                  typename
                  before
                  after
                  constructorname
                  (ParamTypes (S1 ms (Rec0 a)) '[])
              )
          )
          (HasKindOfType a)
      )
      (EnsureRecordType typename before after constructorname (S1 ms (Rec0 a)) (HasKindOfType a))

type family Constructors (t :: Type -> Type) :: [Type -> Type] where
  Constructors (a :+: b) = Append (Constructors a) (Constructors b)
  Constructors a = '[a]

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': (Append xs ys)

type family EnsureRecord typename constructorname params t where
  EnsureRecord typename constructorname params RecordType = ()
  EnsureRecord typename constructorname params t =
    TypeError (MustUseRecordNotationError typename constructorname params)

type family EnsureRecordType typename before after constructorname params t where
  EnsureRecordType typename before after constructorname params RecordType = ()
  EnsureRecordType typename before after constructorname params t =
    TypeError
      ( MustUseRecordTypeInsteadOfParams
          typename
          before
          after
          constructorname
          (ParamTypes params '[])
      )

type family
  FieldsHaveWireTypes
    (typename :: Symbol)
    (constructorname :: Symbol)
    (position :: PositionInList)
    (t :: Type -> Type) ::
    Type
  where
  FieldsHaveWireTypes typename constructorname position (a :*: b) =
    Seq
      (FieldsHaveWireTypes typename constructorname (AddElemAfter position) a)
      (FieldsHaveWireTypes typename constructorname (AddElemBefore position) b)
  FieldsHaveWireTypes typename constructorname position (S1 ('MetaSel ('Just fieldname) u s l) (Rec0 a)) =
    Seq
      ( WhenStuck
          ( TypeError
              ( FieldMustBeWireTypeError
                  '( 'TypeName typename,
                     'ConstructorName constructorname,
                     'FieldName fieldname,
                     position
                   )
                  a
              )
          )
          (HasKindOfType a)
      )
      ()

type family ParamTypes (t :: Type -> Type) (acc :: [Type]) :: [Type] where
  ParamTypes (a :*: b) acc = ParamTypes a (ParamTypes b acc)
  ParamTypes (S1 m (Rec0 t)) acc = t ': acc

type family FieldTypes (t :: Type -> Type) (acc :: [k]) :: [k] where
  FieldTypes (a :*: b) acc = FieldTypes a (FieldTypes b acc)
  FieldTypes (S1 ('MetaSel ('Just fieldname) u s l) (Rec0 t)) acc = (FieldType fieldname t) ': acc

data FieldType (key :: Symbol) (t :: Type)

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

type AtLeastOneConstructorError f =
  "I can't create a Wire instance for this type:"
    % ""
    % Indent ("data " <> f)
    % ""
    % "I need a type to have at least one constructor."
    % ""

type MustUseRecordNotationError (typename :: Symbol) (constructorname :: Symbol) (params :: [Type]) =
  "I can't create a Wire instance for this type:"
    % ""
    % Indent
        ( "data " <> typename
            % Indent ("= " <> constructorname <> " " <> PrintParams params)
        )
    % ""
    % "I'd like field names for all types used in constructors,"
    % "so you can make backwards-compatible changes to your types."
    % "Try using record syntax:"
    % ""
    % Indent ("data " <> typename <> " = " <> constructorname % Indent (PrintParamsAsFields params))
    % ""
    % "But come up with some better field names than x or y!"
    % ""

type MustUseRecordTypeInsteadOfParams
  (typename :: Symbol)
  (before :: Bool)
  (after :: Bool)
  (constructorname :: Symbol)
  (params :: [Type]) =
  "I can't create a Wire instance for this type:"
    % ""
    % Indent
        ( "data " <> typename
            % Indent
                ( FrameRelevantConstructor
                    before
                    after
                    (constructorname <> " " <> PrintParams params)
                )
        )
    % ""
    % "I only support constructors with no parameters, or with a"
    % "a single parameter that must also be a record."
    % "This is to make it easier for you to make changes to your"
    % "types in the future, in a backwards-compatible way."
    % "Try creating a custom record type:"
    % ""
    % Indent
        ( "data " <> typename
            % Indent
                ( FrameRelevantConstructor
                    before
                    after
                    ( constructorname <> " " <> constructorname <> "Record"
                    )
                )
            % ""
            % "data " <> constructorname <> "Record = " <> constructorname <> "Record"
            % Indent (PrintParamsAsFields params)
        )
    % ""

type family
  FrameRelevantConstructor
    (before :: Bool)
    (after :: Bool)
    (constructor :: ErrorMessage) ::
    ErrorMessage
  where
  FrameRelevantConstructor 'False 'False current =
    "= " <> current
  FrameRelevantConstructor 'True 'False current =
    "= ..."
      % "| " <> current
  FrameRelevantConstructor 'False 'True current =
    "= " <> current
      % "| ..."
  FrameRelevantConstructor 'True 'True current =
    "= ..."
      % "| " <> current
      % "| ..."

type UseSeparateRecordType
  (typename :: Symbol)
  (before :: Bool)
  (after :: Bool)
  (constructorname :: Symbol)
  (fields :: [Type]) =
  "I can't create a Wire instance for this type:"
    % ""
    % Indent
        ( "data " <> typename
            % Indent
                ( FrameRelevantConstructor
                    before
                    after
                    (constructorname % Indent (PrintFields fields))
                )
        )
    % ""
    % "I only support constructors with no parameters, or with a"
    % "a single parameter that must a separate record type."
    % "This is to make it easier for you to make changes to your"
    % "types in the future, in a backwards-compatible way."
    % "Try creating a custom record type:"
    % ""
    % Indent
        ( "data " <> typename
            % Indent
                ( FrameRelevantConstructor
                    before
                    after
                    ( constructorname <> " " <> constructorname <> "Record"
                    )
                )
            % ""
            % "data " <> constructorname <> "Record = " <> constructorname <> "Record"
            % Indent (PrintFields fields)
        )
    % ""

type family PrintParams (params :: [Type]) :: GHC.TypeLits.ErrorMessage where
  PrintParams '[a] = ToErrorMessage a
  PrintParams '[a, b] = a <> " " <> b
  PrintParams (a ': b ': rest) = a <> " " <> b <> " ..."

type family PrintParamsAsFields (params :: [Type]) :: GHC.TypeLits.ErrorMessage where
  PrintParamsAsFields '[a] = "{ x :: " <> ToErrorMessage a % "}"
  PrintParamsAsFields '[a, b] = "{ x :: " <> a % ", y :: " <> b % "}"
  PrintParamsAsFields (a ': b ': rest) = "{ x :: " <> a % ", y :: " <> b % ", ..." % "}"

type family PrintFields (fields :: [Type]) :: GHC.TypeLits.ErrorMessage where
  PrintFields '[FieldType keyA a] =
    "{ " <> keyA <> " :: " <> a
      % "}"
  PrintFields '[FieldType keyA a, FieldType keyB b] =
    "{ " <> keyA <> " :: " <> a
      % ", " <> keyB <> " :: " <> b
      % "}"
  PrintFields (FieldType keyA a ': FieldType keyB b ': rest) =
    "{ " <> keyA <> " :: " <> a
      % ", " <> keyB <> " :: " <> b
      % ", ..."
      % "}"

type FieldMustBeWireTypeError
  (context :: RecordFieldContext)
  (a :: Type) =
  "I'm trying to make a Wire instance of this type:"
    % ""
    % Indent (PrintRecordField context (ToErrorMessage a))
    % ""
    % NoWireInstanceForType a
    % ""

data TypeName = TypeName Symbol

data ConstructorName = ConstructorName Symbol

data FieldName = FieldName Symbol

data PositionInList = OnlyElem | FirstElem | MiddleElem | LastElem

type family AddElemBefore (position :: PositionInList) :: PositionInList where
  AddElemBefore 'OnlyElem = 'LastElem
  AddElemBefore 'FirstElem = 'MiddleElem
  AddElemBefore 'MiddleElem = 'MiddleElem
  AddElemBefore 'LastElem = 'LastElem

type family AddElemAfter (position :: PositionInList) :: PositionInList where
  AddElemAfter 'OnlyElem = 'FirstElem
  AddElemAfter 'FirstElem = 'FirstElem
  AddElemAfter 'MiddleElem = 'MiddleElem
  AddElemAfter 'LastElem = 'MiddleElem

type RecordFieldContext =
  ( TypeName,
    ConstructorName,
    FieldName,
    PositionInList
  )

type family
  PrintRecordField
    (context :: RecordFieldContext)
    (a :: ErrorMessage) ::
    ErrorMessage
  where
  PrintRecordField
    '( 'TypeName typename,
       'ConstructorName constructorname,
       'FieldName fieldname,
       position
     )
    fieldType =
    "data " <> typename <> " = " <> constructorname
      % Indent (FrameFields position (fieldname <> " :: " <> fieldType))

type family NoWireInstanceForType (a :: Type) :: ErrorMessage where
  NoWireInstanceForType (a, b) =
    "I don't support tuples in field types, because it's hard to change them"
      % "in the future in a backwards-compatible fashion."
  NoWireInstanceForType (a -> b) =
    "I don't support functions in field types, because I don't know how to"
      % "encode them to JSON."
  NoWireInstanceForType a =
    "I need all the field types to have a Wire instance themselves,"
      % "but miss an instance for the type: " <> a

type family
  FrameFields
    (position :: PositionInList)
    (a :: ErrorMessage) ::
    ErrorMessage
  where
  FrameFields 'OnlyElem field =
    "{ " <> field
      % "}"
  FrameFields 'LastElem field =
    "{ ..."
      % ", " <> field
      % "}"
  FrameFields 'MiddleElem field =
    "{ ..."
      % ", " <> field
      % ", ..."
      % "}"
  FrameFields 'FirstElem field =
    "{ " <> field
      % ", ..."
      % "}"

type ParameterMustBeWireTypeError (typename :: Symbol) (param :: Type) =
  "Before I can make a Wire instance for this type:"
    % ""
    % Indent (ToErrorMessage typename)
    % ""
    % "I need Wire instances for all types used in its constructors,"
    % "but I'm missing a Wire instance for:"
    % ""
    % Indent (ToErrorMessage param)
    % ""

type NoGenericInstanceError (a :: Type) =
  "I'm trying to make a wire instance for this type:"
    % ""
    % Indent ("data " <> a <> " = ...")
    % ""
    % "I need a Generic instance of this type to learn more about it."
    % "Add one like this:"
    % ""
    % Indent
        ( "data " <> a <> " = ..."
            % Indent (ToErrorMessage "deriving (Generic)")
        )
    % ""

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
  Indent ('GHC.TypeLits.ShowType a) = 'GHC.TypeLits.Text "  " ':<>: 'GHC.TypeLits.ShowType a
  Indent ('GHC.TypeLits.Text a) = 'GHC.TypeLits.Text "  " ':<>: 'GHC.TypeLits.Text a
