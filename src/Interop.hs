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
    Interop.Ruby.generateRubyClient,

    -- * Check compatibility between services
    Interop.Compatibility.spec,
    Interop.Compatibility.checkServerClientCompatibility,
  )
where

import qualified Interop.Compatibility
import qualified Interop.Ruby
import qualified Interop.Service
import qualified Interop.Wire
