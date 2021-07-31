{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}

-- | Generation of ruby client code for talking to Haskell backends defined
-- using this library.
module Interop.Ruby (generateRubyClient) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy
import Data.Char as Char
import Data.Function ((&))
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Semigroup (stimesMonoid)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Interop.Service as Service
import qualified Interop.Spec as Spec
import qualified Interop.Wire as Wire
import qualified Interop.Wire.Flat as Flat
import qualified System.IO
import Prelude hiding (pure, (>>), (>>=))

generateRubyClient :: FilePath -> [Text] -> Service.Service m -> IO ()
generateRubyClient path namespaces service =
  System.IO.withFile path System.IO.WriteMode $
    \handle ->
      toCode namespaces service (Service.customTypes service)
        & render
        & Builder.hPutBuilder handle

toCode :: [Text] -> Service.Service m -> [Flat.CustomType] -> Ruby
toCode namespaces service types' = do
  let (moduleNames, apiName) =
        case reverse namespaces of
          [] -> ([], "Api")
          first : rest -> (rest, first)
  "require \"json\""
  "require \"net/http\""
  "require \"uri\""
  "require \"sorbet-runtime\""
  ""
  foldl'
    inNamespace
    (apiClass apiName service types')
    moduleNames
  "# INTEROP-SPEC:" >< fromByteString (Aeson.encode (Spec.spec service))

inNamespace :: Ruby -> Text -> Ruby
inNamespace ruby namespace = do
  "module " >< fromText namespace >| do
    ruby
  "end"

apiClass :: Text -> Service.Service m -> [Flat.CustomType] -> Ruby
apiClass apiName service types' = do
  "class " >< fromText apiName >| do
    ""
    "extend T::Sig"
    "extend T::Helpers"
    -- We define all classes first, then reopen them and add implementations.
    -- This is necessary to allow mutually recursive references between classes.
    forRuby types' (customTypeHead apiName)
    forRuby types' customType
    ""
    "def initialize(origin, headers: {}, timeout: nil)" >| do
      "@origin = URI(origin)"
      "@headers = headers"
      "@http = Net::HTTP.new(@origin.host, @origin.port)"
      ""
      "unless timeout.nil?" >| do
        "@http.open_timeout = timeout"
        "@http.read_timeout = timeout"
      "end"
      "@http.use_ssl = @origin.scheme == 'https'"
    "end"
    forRuby (Map.toList (Service.endpoints service)) (uncurry endpointMethod)
  "end"

customTypeHead :: Text -> Flat.CustomType -> Ruby
customTypeHead apiName (Flat.CustomType typeName (Right constructors)) = do
  ""
  "module " >< fromText typeName >| do
    "extend T::Sig"
    "extend T::Helpers"
    "sealed!"
    ""
    forRuby constructors $ \(Flat.Constructor constructorName _) -> do
      "class "
        >< fromText constructorName
        >< " < T::Struct; include "
        >< fromText apiName
        >< "::"
        >< fromText typeName
        >< "; end"
    ""
    "sig { params(json: Hash).returns(T.self_type) }"
    "def self.from_h(json)" >| do
      "ctor_name, ctor_json = json.first"
      "case ctor_name" >| do
        forRuby constructors $ \(Flat.Constructor constructorName _fields) ->
          "when \"" >< fromText constructorName >< "\"" >| do
            fromText constructorName >< ".from_h(ctor_json)"
      "end"
    "end"
  "end"
customTypeHead _ (Flat.CustomType typeName (Left _)) = do
  ""
  "class " >< fromText typeName >< " < T::Struct; end"

customType :: Flat.CustomType -> Ruby
customType (Flat.CustomType typeName (Right constructors)) = do
  forRuby constructors $ \(Flat.Constructor constructorName fields) -> do
    ""
    "class " >< fromText typeName >< "::" >< fromText constructorName >| do
      "extend T::Sig"
      "extend T::Helpers"
      ""
      forRuby fields $ \(Flat.Field fieldName fieldType) ->
        "prop :" >< toSnakeCase fieldName >< ", " >< type_ fieldType
      ""
      "sig { returns(Hash) }"
      "def to_h" >| do
        "Hash[\"" >< fromText constructorName >< "\", {" >| do
          forRuby fields $ \(Flat.Field fieldName fieldType) ->
            "\""
              >< fromText fieldName
              >< "\": "
              >< encodeJson (toSnakeCase fieldName) fieldType
              >< ","
        "}]"
      "end"
      ""
      "sig { params(json: Hash).returns(T.self_type) }"
      "def self.from_h(json)" >| do
        "new(" >| do
          forRuby fields $ \(Flat.Field fieldName fieldType) ->
            toSnakeCase fieldName
              >< ": "
              >< parseJson (fromText ("json[\"" <> fieldName <> "\"]")) fieldType
              >< ","
        ")"
      "end"
    "end"
customType (Flat.CustomType typeName (Left fields)) = do
  ""
  "class " >< fromText typeName >| do
    "extend T::Sig"
    "extend T::Helpers"
    ""
    forRuby fields $ \(Flat.Field fieldName fieldType) ->
      "prop :" >< toSnakeCase fieldName >< ", " >< type_ fieldType
    ""
    "sig { returns(Hash) }"
    "def to_h" >| do
      "{" >| do
        forRuby fields $ \(Flat.Field fieldName fieldType) ->
          "\""
            >< fromText fieldName
            >< "\": "
            >< encodeJson (toSnakeCase fieldName) fieldType
            >< ","
      "}"
    "end"
    ""
    "sig { params(json: Hash).returns(T.self_type) }"
    "def self.from_h(json)" >| do
      "new(" >| do
        forRuby fields $ \(Flat.Field fieldName fieldType) ->
          toSnakeCase fieldName
            >< ": "
            >< parseJson (fromText ("json[\"" <> fieldName <> "\"]")) fieldType
            >< ","
      ")"
    "end"
  "end"

parseJson :: Ruby -> Flat.Type -> Ruby
parseJson jsonVar type' =
  case type' of
    Flat.Optional sub ->
      jsonVar
        >< " && "
        >< parseJson jsonVar sub
    Flat.List sub ->
      "("
        >< jsonVar
        >< " || []).map { |elem| "
        >< parseJson "elem" sub
        >< " }"
    Flat.Dict key val ->
      "("
        >< jsonVar
        >< " || []).map { |key, val| ["
        >< parseJson "key" key
        >< ", "
        >< parseJson "val" val
        >< "] }.to_h"
    Flat.Text ->
      jsonVar
    Flat.Int ->
      jsonVar
    Flat.Float ->
      jsonVar
    Flat.Bool ->
      jsonVar
    Flat.Unit ->
      "nil"
    Flat.NestedCustomType varName ->
      fromText varName >< ".from_h(" >< jsonVar >< ")"

endpointMethod :: Text -> Service.Endpoint m -> Ruby
endpointMethod name (Service.Endpoint _ _ (_ :: req -> m res)) = do
  let responseType = Flat.fromFieldType (Wire.type_ (Proxy :: Proxy res))
  let requestType = Flat.fromFieldType (Wire.type_ (Proxy :: Proxy req))
  ""
  "sig { params(arg: "
    >< type_ requestType
    >< ", headers: T::Hash[String, String]"
    >< ").returns("
    >< type_ responseType
    >< ") }"
  "def " >< toSnakeCase name >< "(arg, headers: {})" >| do
    "req = Net::HTTP::Post.new(@origin, @headers.merge(headers))"
    "req[\"Content-Type\"] = \"application/json\""
    ""
    "body = [\"" >< fromText name >< "\", " >< encodeJson "arg" requestType >< "]"
    "res = @http.request(req, body.to_json)"
    "json = JSON.parse(res.body)"
    parseJson "json" responseType
  "end"

type_ :: Flat.Type -> Ruby
type_ t =
  case t of
    Flat.Optional sub -> "T.nilable(" >< type_ sub >< ")"
    Flat.List sub -> "T::Array[" >< type_ sub >< "]"
    Flat.Dict key val -> "T::Hash[" >< type_ key >< ", " >< type_ val >< "]"
    Flat.Text -> "String"
    Flat.Int -> "Integer"
    Flat.Float -> "Float"
    Flat.Bool -> "T::Boolean"
    Flat.Unit -> "NilClass"
    Flat.NestedCustomType name -> fromText name

encodeJson :: Ruby -> Flat.Type -> Ruby
encodeJson jsonVar t =
  case t of
    Flat.Optional sub ->
      "if "
        >< jsonVar
        >< ".nil? then {} else "
        >< encodeJson jsonVar sub
        >< " end"
    Flat.List sub ->
      jsonVar
        >< ".map { |elem| "
        >< encodeJson "elem" sub
        >< " }"
    Flat.Dict key val ->
      jsonVar
        >< ".map { |key, val| ["
        >< encodeJson "key" key
        >< ", "
        >< encodeJson "val" val
        >< "] }"
    Flat.Text -> jsonVar
    Flat.Int -> jsonVar
    Flat.Float -> jsonVar
    Flat.Bool -> jsonVar
    Flat.Unit -> "[]"
    Flat.NestedCustomType _ -> jsonVar >< ".to_h"

-- DSL for generating ruby code from Haskell.

newtype Ruby = Ruby (Int -> Builder.Builder)

instance IsString Ruby where
  fromString str =
    Ruby (\_ -> Builder.stringUtf8 str)

(><) :: Ruby -> Ruby -> Ruby
(><) (Ruby x) (Ruby y) = Ruby (x <> y)

(>|) :: Ruby -> Ruby -> Ruby
(>|) line block = do
  line
  indent ("  " >< block)

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

fromText :: Text -> Ruby
fromText text = Ruby (\_ -> Builder.byteString (Data.Text.Encoding.encodeUtf8 text))

fromByteString :: Data.ByteString.Lazy.ByteString -> Ruby
fromByteString bytestring = Ruby (\_ -> Builder.lazyByteString bytestring)

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
