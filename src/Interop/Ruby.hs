{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Interop.Ruby (generate) where

import qualified Data.ByteString.Builder as Builder
import Data.Function ((&))
import Data.List (intersperse)
import Data.Semigroup (stimesMonoid)
import Data.String (IsString (..))
import Interop
import qualified System.IO
import Prelude hiding (return, (>>), (>>=))

generate :: FilePath -> Service m -> IO ()
generate path service =
  System.IO.withFile path System.IO.WriteMode $
    \handle ->
      toCode service
        & render
        & Builder.hPutBuilder handle

toCode :: Service m -> Ruby
toCode _ = do
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

Ruby x >> Ruby y = Ruby (x <> y)

pure = ""

render :: Ruby -> Builder.Builder
render (Ruby f) = f 0
