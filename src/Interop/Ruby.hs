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
    ""
    "module JsonSerialization" do
      "sig { returns(String) }"
      "def to_json" do
        ("Hash[class.class_name, serialize].to_json" :: Ruby)
      "end"
      ""
      "sig { params(json: String).returns(T.self_type) }"
      "def self.from_json(json)" do
        "parsed = JSON.parse(json)"
        "klass = \"#{class_name}::#{parsed[parsed.keys.first]}\".constantize"
        "klass.new(parsed)"
      "end"
      ""
      "sig { returns(String) }"
      "def self.class_name" do
        ("to_s.split(\"::\").last" :: Ruby)
      "end"
    "end"
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
  chunks ["module ", Text.unpack typeName] do
    "sealed!"
    "include JsonSerialization"
    "extend JsonSerialization"
    mapRuby
      ( \(Flat.Constructor constructorName fields) -> do
          ""
          chunks ["class ", fromString (Text.unpack constructorName), " < T::Struct"] do
            "include " <> fromString (Text.unpack typeName)
            ""
            mapRuby
              ( \(Flat.Field fieldName fieldType) ->
                  "prop :" <> fromString (toSnakeCase fieldName) <> ", " <> type_ fieldType
              )
              fields
          "end"
      )
      constructors
  "end"

endpoint :: Text -> Endpoint m -> Ruby
endpoint name (Endpoint _ (_ :: req -> m res)) = do
  let responseType = Flat.fromFieldType (Wire.type_ (Proxy :: Proxy res))
  ""
  "sig { params(body: "
    <> type_ (Flat.fromFieldType (Wire.type_ (Proxy :: Proxy req)))
    <> ").returns("
    <> type_ responseType
    <> ") }"
  chunks ["def ", toSnakeCase name, "(body:)"] do
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
    Flat.NestedCustomType name -> fromString (Text.unpack name)

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
    Flat.NestedCustomType name -> fromString (Text.unpack name) <> ".parse_json"

-- DSL for generating ruby code from Haskell.

newtype Ruby = Ruby (Int -> Builder.Builder)
  deriving (Semigroup, Monoid)

instance IsString Ruby where
  fromString str =
    Ruby (\_ -> Builder.stringUtf8 str)

instance IsString (Ruby -> Ruby) where
  fromString str =
    ( \block -> do
        fromString str
        indent ("  " <> block)
    )

class Chunks a where
  chunks :: [String] -> a

instance Chunks Ruby where
  chunks strs = fromString (mconcat strs)

instance Chunks (Ruby -> Ruby) where
  chunks strs =
    ( \block -> do
        chunks strs
        indent ("  " <> block)
    )

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
