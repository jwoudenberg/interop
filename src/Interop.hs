{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Interop
  ( -- * Creating services
    Interop.Service.service,
    Interop.Service.endpoint,
    Interop.Wire.Wire,
    Interop.Service.Endpoint,
    Interop.Service.Service,

    -- * Running services
    Interop.Service.convert,
    Interop.Service.wai,

    -- * Client code generation
    generateRubyClient,

    -- * Check compatibility between services
    checkServerClientCompatibility,
  )
where

import Data.Text (Text)
import qualified Interop.Diff
import qualified Interop.Ruby
import qualified Interop.Service
import qualified Interop.Wire

generateRubyClient :: FilePath -> [Text] -> Interop.Service.Service m -> IO ()
generateRubyClient = Interop.Ruby.generate

checkServerClientCompatibility :: Interop.Service.Service m -> Interop.Service.Service n -> Text
checkServerClientCompatibility = Interop.Diff.check
