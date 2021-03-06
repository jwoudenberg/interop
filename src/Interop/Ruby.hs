{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Interop.Ruby (generate) where

import qualified Data.ByteString.Builder as Builder
import Data.Char as Char
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.Semigroup (stimesMonoid)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Interop
import qualified System.IO
import Prelude hiding (pure, (>>), (>>=))

generate :: FilePath -> Service m -> IO ()
generate path service' =
  System.IO.withFile path System.IO.WriteMode $
    \handle ->
      toCode service'
        & render
        & Builder.hPutBuilder handle

toCode :: Service m -> Ruby
toCode (Service endpointMap) = do
  "require \"json\""
  "require \"net/http\""
  "require \"uri\""
  ""
  "class Api" $ do
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
    foldr (>>) pure (fmap endpoint (HM.keys endpointMap))
  "end"

endpoint :: Text -> Ruby
endpoint name = do
  ""
  chunks ["def ", toSnakeCase name, "(body:, authorization:)"] $ do
    "req = Net::HTTP::Post.new(@origin)"
    "req[\"Content-Type\"] = \"application/json\""
    "req[\"Authorization\"] = authorization"
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
