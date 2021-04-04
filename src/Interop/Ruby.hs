{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}

-- | Generation of ruby client code for talking to Haskell backends defined
-- using this library.
module Interop.Ruby (generate) where

import qualified Data.ByteString.Builder as Builder
import Data.Char as Char
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Semigroup (stimesMonoid)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Interop
import qualified Interop.Wire as Wire
import qualified Interop.Wire.Flat as Flat
import qualified System.IO
import Prelude hiding (pure, (>>), (>>=))

generate :: FilePath -> Service m -> IO ()
generate path service' =
  case types service' of
    Left err -> fail (Text.unpack err)
    Right types' ->
      System.IO.withFile path System.IO.WriteMode $
        \handle ->
          toCode service' types'
            & render
            & Builder.hPutBuilder handle

toCode :: Service m -> [Flat.CustomType] -> Ruby
toCode service' types' = do
  "require \"json\""
  "require \"net/http\""
  "require \"uri\""
  "require \"sorbet-runtime\""
  ""
  "module Api" do
    ""
    "extend T::Sig"
    "extend T::Helpers"
    forRuby types' customType
    ""
    "def initialize(origin, timeout = nil)" do
      "@origin = URI(origin)"
      "@http = Net::HTTP.new(@origin.host, @origin.port)"
      ""
      "unless timeout.nil?" do
        "@http.open_timeout = timeout"
        "@http.read_timeout = timeout"
      "end"
      "@http.use_ssl = @origin.scheme == 'https'"
    "end"
    forRuby (Map.toList (unService service')) (uncurry endpoint)
  "end"

customType :: Flat.CustomType -> Ruby
customType (Flat.CustomType typeName constructors) = do
  ""
  "module " >< fromText typeName do
    "sealed!"
    forRuby constructors \(Flat.Constructor constructorName fields) -> do
      ""
      "class " >< fromText constructorName >< " < T::Struct" do
        "include " >< fromText typeName
        ""
        forRuby fields \(Flat.Field fieldName fieldType) ->
          "prop :" >< toSnakeCase fieldName >< ", " >< type_ fieldType
        ""
        "sig { returns(Hash) }"
        "def to_h" do
          "Hash[\"" >< fromText constructorName >< "\", {" do
            forRuby fields \(Flat.Field fieldName fieldType) ->
              "\""
                >< fromText fieldName
                >< "\": "
                >< encodeFieldType fieldName fieldType
                >< ","
          "}]"
        "end"
        ""
        "sig { params(json: Hash).returns(T.self_type) }"
        "def self.from_h(json)" do
          "new(" do
            forRuby fields \(Flat.Field fieldName fieldType) ->
              toSnakeCase fieldName
                >< ": "
                >< decodeFieldType "json" fieldName fieldType
                >< ","
          ")"
        "end"
      "end"
    ""
    "sig { params(json: Hash).returns(T.self_type) }"
    "def self.from_h(json)" do
      "ctor_name, ctor_json = json.first"
      "case ctor_name" do
        forRuby constructors \(Flat.Constructor constructorName _fields) ->
          "when \""
            >< fromText constructorName
            >< "\": "
            >< fromText constructorName
            >< ".from_h(ctor_json)"

      "end"
    "end"
  "end"

encodeFieldType :: Text -> Flat.Type -> Ruby
encodeFieldType fieldName fieldType =
  case fieldType of
    Flat.Optional sub ->
      encodeFieldType fieldName sub
        >< " unless "
        >< toSnakeCase fieldName
        >< ".nil?"
    Flat.List sub ->
      toSnakeCase fieldName
        >< ".map { |elem| "
        >< encodeFieldType "elem" sub
        >< " }"
    Flat.Text ->
      toSnakeCase fieldName
    Flat.Int ->
      toSnakeCase fieldName
    Flat.Float ->
      toSnakeCase fieldName
    Flat.Bool ->
      toSnakeCase fieldName
    Flat.Unit ->
      "[]"
    Flat.NestedCustomType _ ->
      toSnakeCase fieldName >< ".to_h"

decodeFieldType :: Text -> Text -> Flat.Type -> Ruby
decodeFieldType jsonVar fieldName fieldType =
  case fieldType of
    Flat.Optional sub ->
      decodeFieldType jsonVar fieldName sub
        >< " unless "
        >< fromText jsonVar
        >< "[\""
        >< fromText fieldName
        >< "\"]"
        >< ".empty?"
    Flat.List sub ->
      fromText jsonVar
        >< "[\""
        >< fromText fieldName
        >< "\"].map { |elem| "
        >< decodeFieldType "elem" fieldName sub
        >< " }"
    Flat.Text ->
      fromText jsonVar >< "[\"" >< fromText fieldName >< "\"]"
    Flat.Int ->
      fromText jsonVar >< "[\"" >< fromText fieldName >< "\"]"
    Flat.Float ->
      fromText jsonVar >< "[\"" >< fromText fieldName >< "\"]"
    Flat.Bool ->
      fromText jsonVar >< "[\"" >< fromText fieldName >< "\"]"
    Flat.Unit ->
      "nil"
    Flat.NestedCustomType varName ->
      fromText varName >< ".from_h(" >< fromText jsonVar >< ")"

endpoint :: Text -> Endpoint m -> Ruby
endpoint name (Endpoint _ (_ :: req -> m res)) = do
  let responseType = Flat.fromFieldType (Wire.type_ (Proxy :: Proxy res))
  ""
  "sig { params(body: "
    >< type_ (Flat.fromFieldType (Wire.type_ (Proxy :: Proxy req)))
    >< ").returns("
    >< type_ responseType
    >< ") }"
  "def " >< toSnakeCase name >< "(body:)" do
    "req = Net::HTTP::Post.new(@origin)"
    "req[\"Content-Type\"] = \"application/json\""
    ""
    "res = @http.request(req, body)"
    "json = JSON.parse(res.body)"
    parseJson "json" responseType
  "end"

type_ :: Flat.Type -> Ruby
type_ t =
  case t of
    Flat.Optional sub -> "T.nilable(" >< type_ sub >< ")"
    Flat.List sub -> "T::Array[" >< type_ sub >< "]"
    Flat.Text -> "String"
    Flat.Int -> "Integer"
    Flat.Float -> "Float"
    Flat.Bool -> "T::Boolean"
    Flat.Unit -> "NilClass"
    Flat.NestedCustomType name -> fromText name

parseJson :: Text -> Flat.Type -> Ruby
parseJson jsonVar t =
  case t of
    Flat.Optional sub ->
      parseJson jsonVar sub
        >< " unless "
        >< fromText jsonVar
        >< ".empty?"
    Flat.List sub ->
      fromText jsonVar
        >< ".map { |elem| "
        >< parseJson "elem" sub
        >< " }"
    Flat.Text -> fromText jsonVar
    Flat.Int -> fromText jsonVar
    Flat.Float -> fromText jsonVar
    Flat.Bool -> fromText jsonVar
    Flat.Unit -> fromText jsonVar
    Flat.NestedCustomType name -> fromText name >< ".from_h(" >< fromText jsonVar >< ")"

-- DSL for generating ruby code from Haskell.

newtype Ruby = Ruby (Int -> Builder.Builder)

instance IsString Ruby where
  fromString str =
    Ruby (\_ -> Builder.stringUtf8 str)

instance IsString (Ruby -> Ruby) where
  fromString str = fromRuby (fromString str)

class FromRuby ruby where
  fromRuby :: Ruby -> ruby

instance FromRuby Ruby where
  fromRuby = id

instance FromRuby (Ruby -> Ruby) where
  fromRuby ruby =
    ( \block -> do
        ruby
        indent ("  " >< block)
    )

(><) :: FromRuby ruby => Ruby -> Ruby -> ruby
(><) (Ruby x) (Ruby y) = fromRuby (Ruby (x <> y))

(>>) :: Ruby -> Ruby -> Ruby
x >> y = x >< "\n" >< indentation >< y

pure :: Ruby
pure = ""

forRuby :: [a] -> (a -> Ruby) -> Ruby
forRuby [] _ = pure
forRuby (x : xs) f = foldr (>>) (f x) (fmap f xs)

indent :: Ruby -> Ruby
indent (Ruby block) =
  Ruby (\i -> block (1 + i))

indentation :: Ruby
indentation = Ruby (\i -> stimesMonoid (2 * i) " ")

render :: Ruby -> Builder.Builder
render (Ruby f) = f 0

fromText :: FromRuby ruby => Text -> ruby
fromText = fromRuby . fromString . Text.unpack

toSnakeCase :: Text -> Ruby
toSnakeCase text =
  Text.foldl
    ( \acc char ->
        case (acc, Char.isUpper char) of
          ([], _) -> [Char.toLower char]
          (_, True) -> Char.toLower char : '_' : acc
          (_, False) -> char : acc
    )
    []
    text
    & reverse
    & fromString
