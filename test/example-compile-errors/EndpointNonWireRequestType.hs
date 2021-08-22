{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EndpointNonWireRequestType () where

import GHC.Generics (Generic)
import qualified Interop

endpoint :: Interop.Endpoint IO
endpoint = Interop.endpoint "hi" (\(_ :: NonWireType) -> pure ())

data NonWireType = NonWireType

-- Compilation error:
--
-- • No instance for (Wire NonWireType)
--     arising from a use of ‘endpoint’
-- • In the expression:
--     endpoint "hi" (\ (_ :: NonWireType) -> pure ())
--   In an equation for ‘endpoint’:
--       endpoint = endpoint "hi" (\ (_ :: NonWireType) -> pure ())
