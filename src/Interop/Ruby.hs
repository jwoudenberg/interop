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
    mapRuby customType types'
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
    mapRuby (uncurry endpoint) (Map.toList (unService service'))
  "end"

customType :: Flat.CustomType -> Ruby
customType (Flat.CustomType typeName constructors) = do
  ""
  "module " >< fromString (Text.unpack typeName) do
    "sealed!"
    mapRuby
      ( \(Flat.Constructor constructorName fields) -> do
          let className = fromString (Text.unpack constructorName)
          ""
          "class " >< className >< " < T::Struct" do
            "include " <> fromText typeName
            ""
            mapRuby
              ( \(Flat.Field fieldName fieldType) ->
                  "prop :" <> fromString (toSnakeCase fieldName) <> ", " <> type_ fieldType
              )
              fields
            ""
            "sig { returns(Hash) }"
            "def to_h" do
              "Hash[\"" >< className >< "\", {" do
                mapRuby
                  ( \(Flat.Field fieldName fieldType) ->
                      "\""
                        <> fromText fieldName
                        <> "\": "
                        <> encodeFieldType fieldName fieldType
                        <> ","
                  )
                  fields
              "}]"
            "end"
            ""
            "sig { params(json: Hash).returns(T.self_type) }"
            "def self.from_h(json)" do
              "new(" do
                mapRuby
                  ( \(Flat.Field fieldName fieldType) ->
                      fromString (toSnakeCase fieldName)
                        <> ": "
                        <> decodeFieldType "json" fieldName fieldType
                        <> ","
                  )
                  fields
              ")"
            "end"
          "end"
      )
      constructors
    ""
    "sig { params(json: Hash).returns(T.self_type) }"
    "def self.from_h(json)" do
      "ctor_name, ctor_json = json.first"
      "case ctor_name" do
        mapRuby
          ( \(Flat.Constructor constructorName _fields) -> do
              "when \""
                <> fromText constructorName
                <> "\": "
                <> fromText constructorName
                <> ".from_h(ctor_json)"
          )
          constructors

      "end"
    "end"
  "end"

encodeFieldType :: Text -> Flat.Type -> Ruby
encodeFieldType fieldName fieldType =
  case fieldType of
    Flat.Optional sub ->
      encodeFieldType fieldName sub
        <> " unless "
        <> fromString (toSnakeCase fieldName)
        <> ".nil?"
    Flat.List sub ->
      fromString (toSnakeCase fieldName)
        <> ".map { |elem| "
        <> encodeFieldType "elem" sub
        <> " }"
    Flat.Text ->
      fromString (toSnakeCase fieldName)
    Flat.Int ->
      fromString (toSnakeCase fieldName)
    Flat.Float ->
      fromString (toSnakeCase fieldName)
    Flat.Bool ->
      fromString (toSnakeCase fieldName)
    Flat.Unit ->
      "[]"
    Flat.NestedCustomType _ ->
      fromString (toSnakeCase fieldName) <> ".to_h"

decodeFieldType :: Text -> Text -> Flat.Type -> Ruby
decodeFieldType jsonVar fieldName fieldType =
  case fieldType of
    Flat.Optional sub ->
      decodeFieldType jsonVar fieldName sub
        <> " unless "
        <> fromText jsonVar
        <> "[\""
        <> fromText fieldName
        <> "\"]"
        <> ".empty?"
    Flat.List sub ->
      "json[\""
        <> fromText fieldName
        <> "\"].map { |elem| "
        <> decodeFieldType "elem" fieldName sub
        <> " }"
    Flat.Text ->
      fromText jsonVar <> "[\"" <> fromText fieldName <> "\"]"
    Flat.Int ->
      fromText jsonVar <> "[\"" <> fromText fieldName <> "\"]"
    Flat.Float ->
      fromText jsonVar <> "[\"" <> fromText fieldName <> "\"]"
    Flat.Bool ->
      fromText jsonVar <> "[\"" <> fromText fieldName <> "\"]"
    Flat.Unit ->
      "nil"
    Flat.NestedCustomType varName ->
      fromText varName <> ".from_h(" <> fromText jsonVar <> ")"

endpoint :: Text -> Endpoint m -> Ruby
endpoint name (Endpoint _ (_ :: req -> m res)) = do
  let responseType = Flat.fromFieldType (Wire.type_ (Proxy :: Proxy res))
  ""
  "sig { params(body: "
    <> type_ (Flat.fromFieldType (Wire.type_ (Proxy :: Proxy req)))
    <> ").returns("
    <> type_ responseType
    <> ") }"
  "def " >< fromString (toSnakeCase name) >< "(body:)" do
    "req = Net::HTTP::Post.new(@origin)"
    "req[\"Content-Type\"] = \"application/json\""
    ""
    "res = @http.request(req, body)"
    parseJson responseType <> "(res.body)"
  "end"

type_ :: Flat.Type -> Ruby
type_ t =
  case t of
    Flat.Optional sub -> "T.nilable(" <> type_ sub <> ")"
    Flat.List sub -> "T::Array[" <> type_ sub <> "]"
    Flat.Text -> "String"
    Flat.Int -> "Integer"
    Flat.Float -> "Float"
    Flat.Bool -> "T::Boolean"
    Flat.Unit -> "NilClass"
    Flat.NestedCustomType name -> fromText name

parseJson :: Flat.Type -> Ruby
parseJson t =
  case t of
    Flat.Optional _ -> "JSON.parse"
    Flat.List _ -> "JSON.parse"
    Flat.Text -> "JSON.parse"
    Flat.Int -> "JSON.parse"
    Flat.Float -> "JSON.parse"
    Flat.Bool -> "JSON.parse"
    Flat.Unit -> "JSON.parse"
    Flat.NestedCustomType name -> fromText name <> ".parse_json"

-- DSL for generating ruby code from Haskell.

newtype Ruby = Ruby (Int -> Builder.Builder)
  deriving (Semigroup, Monoid)

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
        indent ("  " <> block)
    )

(><) :: FromRuby ruby => Ruby -> Ruby -> ruby
(><) (Ruby x) (Ruby y) = fromRuby (Ruby (x <> y))

(>>) :: Ruby -> Ruby -> Ruby
x >> y = x <> "\n" <> indentation <> y

pure :: Ruby
pure = ""

mapRuby :: (a -> Ruby) -> [a] -> Ruby
mapRuby _ [] = pure
mapRuby f (x : xs) = foldr (>>) (f x) (fmap f xs)

indent :: Ruby -> Ruby
indent (Ruby block) =
  Ruby (\i -> block (1 + i))

indentation :: Ruby
indentation = Ruby (\i -> stimesMonoid (2 * i) " ")

render :: Ruby -> Builder.Builder
render (Ruby f) = f 0

fromText :: Text -> Ruby
fromText = fromString . Text.unpack

toSnakeCase :: Text -> String
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
