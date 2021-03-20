{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}

-- | Generation of ruby client code for talking to Haskell backends defined
-- using this library.
module Interop.Ruby (generate) where

import qualified Data.ByteString.Builder as Builder
import Data.Char as Char
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Semigroup (stimesMonoid)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Interop
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
    ""
    foldr (>>) pure (fmap customType types')
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
    foldr (>>) pure (fmap endpoint (Map.keys (unService service')))
  "end"

customType :: Flat.CustomType -> Ruby
customType type' = do
  chunks ["class", Text.unpack (Flat.typeName type')]
  "end"

endpoint :: Text -> Ruby
endpoint name = do
  ""
  chunks ["def ", toSnakeCase name, "(body:)"] $ do
    "req = Net::HTTP::Post.new(@origin)"
    "req[\"Content-Type\"] = \"application/json\""
    ""
    "@http.request(req, body)"
  "end"

-- DSL for generating ruby code from Haskell.

newtype Ruby = Ruby (Int -> Builder.Builder)

instance IsString Ruby where
  fromString str =
    Ruby
      ( \indent ->
          stimesMonoid (2 * indent) " " <> Builder.stringUtf8 str <> "\n"
      )

instance IsString (Ruby -> Ruby) where
  fromString str =
    ( \(Ruby block) -> do
        fromString str
        (Ruby (\indent -> block (1 + indent)))
    )

class Chunks a where
  chunks :: [String] -> a

instance Chunks Ruby where
  chunks strs = fromString (mconcat strs)

instance Chunks (Ruby -> Ruby) where
  chunks strs =
    ( \(Ruby block) -> do
        chunks strs
        (Ruby (\indent -> block (1 + indent)))
    )

(>>) :: Ruby -> Ruby -> Ruby
Ruby x >> Ruby y = Ruby (x <> y)

pure :: Ruby
pure = ""

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
