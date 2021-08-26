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

-- * Wire instances for existing types.

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

instance TypeError (PrintFunctionNotAWireTypeError (a -> b)) => Wire (a -> b) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b) '[a, b]) => Wire (a, b) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c) '[a, b, c]) => Wire (a, b, c) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d) '[a, b, c, d]) => Wire (a, b, c, d) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e) '[a, b, c, d, e]) => Wire (a, b, c, d, e) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f) '[a, b, c, d, e, f]) => Wire (a, b, c, d, e, f) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g) '[a, b, c, d, e, f, g]) => Wire (a, b, c, d, e, f, g) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h) '[a, b, c, d, e, f, g, h]) => Wire (a, b, c, d, e, f, g, h) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i) '[a, b, c, d, e, f, g, h, i]) => Wire (a, b, c, d, e, f, g, h, i) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j) '[a, b, c, d, e, f, g, h, i, j]) => Wire (a, b, c, d, e, f, g, h, i, j) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k) '[a, b, c, d, e, f, g, h, i, j, k]) => Wire (a, b, c, d, e, f, g, h, i, j, k) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l) '[a, b, c, d, e, f, g, h, i, j, k, l]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l, m) '[a, b, c, d, e, f, g, h, i, j, k, l, m]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l, m, n) '[a, b, c, d, e, f, g, h, i, j, k, l, m, n]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) where
  rec _ = error "unreachable"

instance TypeError (PrintTupleNotAWireTypeError (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]) => Wire (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) where
  rec _ = error "unreachable"

-- * Generics-derived wire instances for user-defined types.

class WireG kindOfType f where
  typeG :: Proxy kindOfType -> Proxy f -> WireType
  encodeG :: Proxy kindOfType -> f a -> Aeson.Encoding
  decodeG :: Proxy kindOfType -> Aeson.Value -> Aeson.Parser (f a)

class CtorsG f where
  typeCtorsG :: Proxy f -> [Constructor]
  encodeCtorsG :: f a -> Aeson.Series
  decodeCtorsG :: Aeson.Object -> Aeson.Parser (f a)

class FieldsG f where
  typeFieldsG :: Proxy f -> [Field]
  encodeFieldsG :: f a -> Aeson.Series
  decodeFieldsG :: Aeson.Object -> Aeson.Parser (f a)

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

-- ** ParseField

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

instance ParseField 'True ()

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
  IsOptional () = 'True
  IsOptional (Maybe a) = 'True
  IsOptional [a] = 'True
  IsOptional (HashMap.HashMap k v) = 'True
  IsOptional (HashSet.HashSet a) = 'True
  IsOptional (Map.Map k v) = 'True
  IsOptional (Set.Set a) = 'True
  IsOptional (Seq.Seq a) = 'True
  IsOptional a = 'False

-- ** Generics-supporting type-level functions

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
      ( ValidateConstructors
          (Constructors ('TypeName typename) 'OnlyElem (a :+: b))
      )
      CustomType
  KindOfType (D1 ('MetaData typename m p f) a) =
    KindOfConstructor ('TypeName typename) 'OnlyElem a

type family
  KindOfConstructor
    (typename :: TypeName)
    (position :: PositionInList)
    (constructor :: Type -> Type) ::
    Type
  where
  KindOfConstructor typename position (C1 ('MetaCons n f b) U1) =
    RecordType
  KindOfConstructor typename position (C1 ('MetaCons constructorname f 'True) a) =
    Seq
      ( FieldsHaveWireTypes
          ('ConstructorContext typename ('ConstructorName constructorname) position)
          'OnlyElem
          a
      )
      RecordType
  KindOfConstructor typename position (C1 ('MetaCons constructorname f 'False) (a :*: b)) =
    TypeError
      ( MustUseRecordNotationError
          typename
          constructorname
          (ParamTypes (a :*: b) '[])
      )
  KindOfConstructor typename position (C1 ('MetaCons constructorname f 'False) (S1 ms (Rec0 a))) =
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
    (xs :: [(ConstructorContext, Bool, Type -> Type)]) ::
    Type
  where
  ValidateConstructors '[] =
    ()
  ValidateConstructors '[ '(context, record, x)] =
    ValidateSingleConstructor context record x
  ValidateConstructors ('(context, record, x) ': rest) =
    Seq
      (ValidateSingleConstructor context record x)
      (ValidateConstructors rest)

type family
  ValidateSingleConstructor
    (context :: ConstructorContext)
    (record :: Bool)
    (x :: Type -> Type) ::
    Type
  where
  ValidateSingleConstructor context record U1 =
    ()
  ValidateSingleConstructor context 'False (a :*: b) =
    TypeError
      ( MustUseRecordTypeInsteadOfParams
          context
          (ParamTypes (a :*: b) '[])
      )
  ValidateSingleConstructor context 'True params =
    TypeError (UseSeparateRecordType context (FieldTypes params '[]))
  ValidateSingleConstructor context 'False (S1 ms (Rec0 a)) =
    Seq
      ( WhenStuck
          ( TypeError
              ( MustUseRecordTypeInsteadOfParams
                  context
                  (ParamTypes (S1 ms (Rec0 a)) '[])
              )
          )
          (HasKindOfType a)
      )
      (EnsureRecordType context (S1 ms (Rec0 a)) (HasKindOfType a))

type family
  Constructors
    (typename :: TypeName)
    (position :: PositionInList)
    (t :: Type -> Type) ::
    [(ConstructorContext, Bool, Type -> Type)]
  where
  Constructors typename position (a :+: b) =
    Append
      (Constructors typename (AddElemAfter position) a)
      (Constructors typename (AddElemBefore position) b)
  Constructors typename position (C1 ('MetaCons ctorname fix rec) a) =
    '[ '( 'ConstructorContext typename ('ConstructorName ctorname) position,
          rec,
          a
        )
     ]

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': (Append xs ys)

type family EnsureRecord typename constructorname params t where
  EnsureRecord typename constructorname params RecordType = ()
  EnsureRecord typename constructorname params t =
    TypeError (MustUseRecordNotationError typename constructorname params)

type family EnsureRecordType context params t where
  EnsureRecordType context params RecordType = ()
  EnsureRecordType context params t =
    TypeError
      ( MustUseRecordTypeInsteadOfParams
          context
          (ParamTypes params '[])
      )

type family
  FieldsHaveWireTypes
    (constructorContext :: ConstructorContext)
    (position :: PositionInList)
    (t :: Type -> Type) ::
    Type
  where
  FieldsHaveWireTypes constructorContext position (a :*: b) =
    Seq
      (FieldsHaveWireTypes constructorContext (AddElemAfter position) a)
      (FieldsHaveWireTypes constructorContext (AddElemBefore position) b)
  FieldsHaveWireTypes constructorContext position (S1 ('MetaSel ('Just fieldname) u s l) (Rec0 a)) =
    Seq
      ( WhenStuck
          ( TypeError
              ( FieldMustBeWireTypeError
                  ( 'RecordFieldContext
                      constructorContext
                      ('FieldName fieldname)
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

-- * Type-level helper functions

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

-- * Custom type errors

data ConstructorContext
  = ConstructorContext
      TypeName
      ConstructorName
      PositionInList

data RecordFieldContext
  = RecordFieldContext
      ConstructorContext
      FieldName
      PositionInList

type AtLeastOneConstructorError f =
  "I can't create a Wire instance for this type:"
    % ""
    % Indent ("data " <> f)
    % ""
    % "I need a type to have at least one constructor."
    % ""

type MustUseRecordNotationError (typename :: TypeName) (constructorname :: Symbol) (params :: [Type]) =
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

type family
  MustUseRecordTypeInsteadOfParams
    (context :: ConstructorContext)
    (params :: [Type]) ::
    ErrorMessage
  where
  MustUseRecordTypeInsteadOfParams
    ('ConstructorContext typename constructorname position)
    params =
    "I can't create a Wire instance for this type:"
      % ""
      % Indent
          ( "data " <> typename
              % Indent
                  ( FrameRelevantConstructor
                      position
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
                      position
                      (constructorname <> " " <> constructorname <> "Record")
                  )
              % ""
              % "data " <> constructorname <> "Record = " <> constructorname <> "Record"
              % Indent (PrintParamsAsFields params)
          )
      % ""

type family
  FrameRelevantConstructor
    (position :: PositionInList)
    (constructor :: ErrorMessage) ::
    ErrorMessage
  where
  FrameRelevantConstructor 'OnlyElem current =
    "= " <> current
  FrameRelevantConstructor 'LastElem current =
    "= ..."
      % "| " <> current
  FrameRelevantConstructor 'FirstElem current =
    "= " <> current
      % "| ..."
  FrameRelevantConstructor 'MiddleElem current =
    "= ..."
      % "| " <> current
      % "| ..."

type family
  UseSeparateRecordType
    (context :: ConstructorContext)
    (fields :: [Type]) ::
    ErrorMessage
  where
  UseSeparateRecordType
    ('ConstructorContext typename constructorname position)
    fields =
    "I can't create a Wire instance for this type:"
      % ""
      % Indent
          ( "data " <> typename
              % Indent
                  ( FrameRelevantConstructor
                      position
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
                      position
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
    % NoWireInstanceForType context a
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

type family
  PrintRecordField
    (context :: RecordFieldContext)
    (a :: ErrorMessage) ::
    ErrorMessage
  where
  PrintRecordField
    ( 'RecordFieldContext
        ( 'ConstructorContext
            typename
            constructorname
            constructorPosition
        )
        fieldname
        position
    )
    fieldType =
    "data " <> typename <> " = " <> constructorname
      % Indent (FrameFields position (fieldname <> " :: " <> fieldType))

type family
  NoWireInstanceForType
    (context :: RecordFieldContext)
    (a :: Type) ::
    ErrorMessage
  where
  NoWireInstanceForType context (a -> b) =
    "I don't support functions in field types, because I don't know how to"
      % "encode them to JSON."
  NoWireInstanceForType context (a, b) =
    PrintTupleInFieldError context '[a, b]
  NoWireInstanceForType context (a, b, c) =
    PrintTupleInFieldError context '[a, b, c]
  NoWireInstanceForType context (a, b, c, d) =
    PrintTupleInFieldError context '[a, b, c, d]
  NoWireInstanceForType context (a, b, c, d, e) =
    PrintTupleInFieldError context '[a, b, c, d, e]
  NoWireInstanceForType context (a, b, c, d, e, f) =
    PrintTupleInFieldError context '[a, b, c, d, e, f]
  NoWireInstanceForType context (a, b, c, d, e, f, g) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l, m) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l, m]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l, m, n]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y]
  NoWireInstanceForType context (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) =
    PrintTupleInFieldError context '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]
  NoWireInstanceForType context a =
    "I need all the field types to have a Wire instance themselves,"
      % "but miss an instance for the type: " <> a

type PrintFunctionNotAWireTypeError (fn :: Type) =
  "You're using a function type in your endpoint:"
    % ""
    % Indent (ToErrorMessage fn)
    % ""
    % "I don't support functions in endpoints types, because I don't know how"
    % "to encode functions to JSON."
    % ""

type PrintTupleNotAWireTypeError (tuple :: Type) (params :: [Type]) =
  "You're using a tuple type in your endpoint: "
    % ""
    % Indent (ToErrorMessage tuple)
    % ""
    % "I don't support tuples as request or response types."
    % ""
    % "I prefer records over tuples, because those will allow you to make"
    % "backwards-compatible changes in the future."
    % "Try using record syntax:"
    % ""
    % Indent
        ( "data MyRecord = MyRecord"
            % Indent (PrintParamsAsFields params)
        )
    % ""
    % "But come up with some better field names than MyRecord, x, and y!"
    % ""

type PrintTupleInFieldError (context :: RecordFieldContext) params =
  "I prefer records over tuples, because those will allow you to make"
    % "backwards-compatible changes in the future."
    % "Try using record syntax:"
    % ""
    % Indent
        ( PrintRecordField context (ToErrorMessage "MyRecord")
            % ""
            % "data MyRecord = MyRecord"
            % Indent (PrintParamsAsFields params)
        )
    % ""
    % "But come up with some better field names than MyRecord, x, and y!"

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

type ParameterMustBeWireTypeError (typename :: TypeName) (param :: Type) =
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

-- * Custom type error helper functions

-- Some helpers for constructing nice type errors. API inspired by the
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
  ToErrorMessage ('TypeName s) = 'GHC.TypeLits.Text s
  ToErrorMessage ('ConstructorName s) = 'GHC.TypeLits.Text s
  ToErrorMessage ('FieldName s) = 'GHC.TypeLits.Text s
  ToErrorMessage t = 'GHC.TypeLits.ShowType t

type family Indent (a :: GHC.TypeLits.ErrorMessage) :: GHC.TypeLits.ErrorMessage where
  Indent (a ':$$: b) = Indent a ':$$: Indent b
  Indent (a ':<>: b) = Indent a ':<>: b
  Indent ('GHC.TypeLits.ShowType a) = 'GHC.TypeLits.Text "  " ':<>: 'GHC.TypeLits.ShowType a
  Indent ('GHC.TypeLits.Text a) = 'GHC.TypeLits.Text "  " ':<>: 'GHC.TypeLits.Text a
