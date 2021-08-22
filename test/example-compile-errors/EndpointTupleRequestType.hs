{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EndpointTupleRequestType () where

import GHC.Generics (Generic)
import qualified Interop

endpoint :: Interop.Endpoint IO
endpoint = Interop.endpoint "hi" (\(_ :: (Int, Bool)) -> pure ())

-- Compilation error:
--
-- • No instance for (Wire (Int, Bool))
--     arising from a use of ‘endpoint’
-- • In the expression:
--     endpoint "hi" (\ (_ :: (Int, Bool)) -> pure ())
--   In an equation for ‘endpoint’:
--       endpoint = endpoint "hi" (\ (_ :: (Int, Bool)) -> pure ())
