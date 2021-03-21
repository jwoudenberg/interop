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
  "module Api" $ do
    ""
    "extend T::Sig"
    "extend T::Helpers"
    mapRuby customType types'
    "def initialize(origin, timeout = nil)" $ do
      "@origin = URI(origin)"
      "@http = Net::HTTP.new(@origin.host, @origin.port)"
      ""
      "unless timeout.nil?" $ do
        "@http.open_timeout = timeout"
        "@http.read_timeout = timeout"
      "end"
      "@http.use_ssl = @origin.scheme == 'https'"
    "end"
    mapRuby (uncurry endpoint) (Map.toList (unService service'))
  "end"

customType :: Flat.CustomType -> Ruby
customType type' = do
  chunks ["class", Text.unpack (Flat.typeName type')]
  "end"

endpoint :: Text -> Endpoint m -> Ruby
endpoint name (Endpoint _ (_ :: req -> m res)) =
  let reqRecordName = toCamelCase name <> "Request"
      resRecordName = toCamelCase name <> "Response"
      (maybeReqRecord, reqType) =
        type_
          reqRecordName
          (Flat.fromFieldType (Wire.type_ (Proxy :: Proxy req)))
      (maybeResRecord, resType) =
        type_
          resRecordName
          (Flat.fromFieldType (Wire.type_ (Proxy :: Proxy res)))
   in do
        maybe mempty (renderRecord reqRecordName) maybeReqRecord
        maybe mempty (renderRecord resRecordName) maybeResRecord
        "sig { params(body: "
          <> reqType
          <> ").returns("
          <> resType
          <> ") }"
        chunks ["def ", toSnakeCase name, "(body:)"] $ do
          "req = Net::HTTP::Post.new(@origin)"
          "req[\"Content-Type\"] = \"application/json\""
          ""
          "@http.request(req, body)"
        "end"

type_ :: Text -> Flat.Type -> (Maybe [Flat.Field], Ruby)
type_ recordName t =
  case t of
    Flat.Optional sub -> fmap (\subName -> "T.nilable(" <> subName <> ")") (type_ recordName sub)
    Flat.List sub -> fmap (\subName -> "T::Array[" <> subName <> "]") (type_ recordName sub)
    Flat.Text -> (Nothing, "String")
    Flat.Int -> (Nothing, "Integer")
    Flat.Float -> (Nothing, "Float")
    Flat.Bool -> (Nothing, "T::Boolean")
    Flat.Unit -> (Nothing, "NilClass")
    Flat.Record fields -> (Just fields, fromString (Text.unpack recordName))
    Flat.NestedCustomType name -> (Nothing, fromString (Text.unpack name))

renderRecord :: Text -> [Flat.Field] -> Ruby
renderRecord recordName fields = do
  ""
  chunks ["class ", fromString (Text.unpack recordName), " < T::Struct"] $
    mapRuby
      ( \(Flat.Field fieldName fieldType) ->
          "prop :" <> fromString (toSnakeCase fieldName) <> ", " <> snd (type_ "???" fieldType)
      )
      fields
  "end"

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

toCamelCase :: Text -> Text
toCamelCase text =
  case Text.uncons text of
    Nothing -> text
    Just (x, rest) -> Text.cons (Char.toUpper x) rest
