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

module Interop where

import Control.Applicative ((<|>))
import qualified Control.Exception
import qualified Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (foldl')
import qualified Data.Foldable as Foldable
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import Data.List (sortOn)
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (toBoundedInteger, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Word
import GHC.Generics hiding (moduleName, packageName)
import GHC.TypeLits hiding (Text)
import qualified GHC.TypeLits
import qualified Network.Wai as Wai

data Endpoint m where
  Endpoint :: (Wire req, Wire res) => (req -> m res) -> Endpoint m

newtype Service m = Service (HM.HashMap Text (Endpoint m))

data WireType
  = Constructors TypeDefinition [(Text, WireType)]
  | Record [(Text, WireType)]
  | List WireType
  | Tuple [WireType]
  | Optional WireType
  | Text
  | Int
  | Float
  | Bool
  deriving (Show)

data TypeDefinition = TypeDefinition
  { packageName :: Text,
    moduleName :: Text,
    typeName :: Text
  }
  deriving (Eq, Show)

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
  type_ _ = Tuple []
  encode _ = Encoding.emptyArray_
  decode _ = pure ()

instance
  ( Wire a,
    Wire b
  ) =>
  Wire (a, b)
  where
  type_ _ =
    Tuple
      [ type_ (Proxy :: Proxy a),
        type_ (Proxy :: Proxy b)
      ]
  encode (a, b) =
    Aeson.pairs $
      Encoding.pair "1" (encode a)
        <> Encoding.pair "2" (encode b)
  decode =
    Aeson.withObject
      "(,)"
      ( \obj ->
          (,)
            <$> Aeson.explicitParseField decode obj "1"
            <*> Aeson.explicitParseField decode obj "2"
      )

instance
  ( Wire a,
    Wire b,
    Wire c
  ) =>
  Wire (a, b, c)
  where
  type_ _ =
    Tuple
      [ type_ (Proxy :: Proxy a),
        type_ (Proxy :: Proxy b),
        type_ (Proxy :: Proxy c)
      ]
  encode (a, b, c) =
    Aeson.pairs $
      Encoding.pair "1" (encode a)
        <> Encoding.pair "2" (encode b)
        <> Encoding.pair "3" (encode c)
  decode =
    Aeson.withObject
      "(,,)"
      ( \obj ->
          (,,)
            <$> Aeson.explicitParseField decode obj "1"
            <*> Aeson.explicitParseField decode obj "2"
            <*> Aeson.explicitParseField decode obj "3"
      )

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
    Constructors
      TypeDefinition
        { packageName = T.pack (symbolVal (Proxy :: Proxy packname)),
          moduleName = T.pack (symbolVal (Proxy :: Proxy modname)),
          typeName = T.pack (symbolVal (Proxy :: Proxy typename))
        }
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
        Tuple []
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
    ( C1
        ('MetaCons ctorname fix 'False)
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
        Record (typeFieldsG (Proxy :: Proxy fields))
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
    ParseField (IsMaybe field) field,
    Wire field,
    Wire (Unwrapped (IsMaybe field) field)
  ) =>
  FieldsG (S1 ('MetaSel ('Just fieldname) unpackedness strictness lazyness) (Rec0 field))
  where
  typeFieldsG _ =
    [ ( T.pack (symbolVal (Proxy :: Proxy fieldname)),
        type_ (Proxy :: Proxy field)
      )
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
  IsMaybe (Maybe a) = True
  IsMaybe a = False

convert :: (forall a. m a -> n a) -> Service m -> Service n
convert nt (Service endpointMap) =
  endpointMap
    & fmap (\(Endpoint f) -> Endpoint (nt . f))
    & Service

data InvalidService
  = InvalidRequestType WireType
  | DuplicateRequestType Text
  deriving (Show)

service :: [Endpoint m] -> Either InvalidService (Service m)
service endpoints =
  Control.Monad.foldM
    ( \endpointMap endpoint ->
        case name (requestType endpoint) of
          Nothing -> Left (InvalidRequestType (requestType endpoint))
          Just cmdName ->
            if HM.member cmdName endpointMap
              then Left (DuplicateRequestType cmdName)
              else Right (HM.insert cmdName endpoint endpointMap)
    )
    HM.empty
    endpoints
    & fmap Service

name :: WireType -> Maybe Text
name wireType =
  case wireType of
    Constructors typeDefinition _ -> Just (typeName typeDefinition)
    _ -> Nothing

requestType :: Endpoint m -> WireType
requestType (Endpoint (f :: req -> m res)) =
  type_ (Proxy :: Proxy req)

data Error
  = ReceivedUnknownCmd Text
  | FailedToParseRequest Text
  deriving (Show)

instance Control.Exception.Exception Error

run :: Monad m => Service m -> ByteString -> (forall a. Error -> m a) -> m ByteString
run (Service endpointMap) reqBytes handleErr = do
  (cmd, payload) <-
    case Aeson.eitherDecode reqBytes of
      Left parseErr -> handleErr (FailedToParseRequest (T.pack parseErr))
      Right parsed -> pure parsed
  case HM.lookup cmd endpointMap of
    Nothing -> handleErr (ReceivedUnknownCmd cmd)
    Just (Endpoint f) -> do
      case Aeson.parseEither decode payload of
        Left parseErr -> handleErr (FailedToParseRequest (T.pack parseErr))
        Right req -> Encoding.encodingToLazyByteString . encode <$> f req

wai :: Service IO -> Wai.Application
wai service =
  \req respond -> do
    reqBytes <- Wai.strictRequestBody req
    res <- run service reqBytes Control.Exception.throwIO
    respond (Wai.responseLBS (toEnum 200) [] res)

data TypeDiff
  = AddedConstructor Text WireType
  | RemovedConstructor Text WireType
  | ChangedConstructor Text TypeDiff
  | AddedField Text WireType
  | RemovedField Text WireType
  | ChangedField Text TypeDiff
  | ChangedType WireType WireType
  | MadeOptional
  | MadeNonOptional

data Path
  = Type Text
  | Constructor Text Path
  | Field Text Path

diffType :: Path -> WireType -> WireType -> [(Path, TypeDiff)]
diffType path (Constructors name1 ctors1) (Constructors name2 ctors2) = undefined

merge ::
  Ord key =>
  (key -> a -> result -> result) ->
  (key -> a -> b -> result -> result) ->
  (key -> b -> result -> result) ->
  [(key, a)] ->
  [(key, b)] ->
  result ->
  result
merge leftOnly both rightOnly left right start =
  undefined mergeHelp start (sortOn fst left) (sortOn fst right)
  where
    mergeHelp acc [] ((k, r) : rs) = mergeHelp (rightOnly k r acc) [] rs
    mergeHelp acc ((k, l) : ls) [] = mergeHelp (leftOnly k l acc) ls []
    mergeHelper acc ((kl, l) : ls) ((kr, r) : rs)
      | kl == kr = mergeHelp (both kl l r acc) ls rs
      | kl < kr = mergeHelp (leftOnly kl l acc) ls ((kr, r) : rs)
      | kl > kr = mergeHelp (rightOnly kr r acc) ((kl, l) : ls) rs

saveSpec :: FilePath -> Service m -> IO ()
saveSpec = undefined

loadSpec :: FilePath -> IO (Service Proxy)
loadSpec = undefined

-- | Lists breaking and non-breaking changes in the API.
changelog :: Service m -> Service n -> Text
changelog = undefined
