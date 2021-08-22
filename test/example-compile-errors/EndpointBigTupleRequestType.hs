{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EndpointBigTupleRequestType () where

import GHC.Generics (Generic)
import qualified Interop

endpoint :: Interop.Endpoint IO
endpoint = Interop.endpoint "hi" (\(_ :: (Int, Float, (), Int, Float, ())) -> pure ())

-- Compilation error:
--
-- • No instance for (Wire (Int, Float, (), Int, Float, ()))
--     arising from a use of ‘endpoint’
-- • In the expression:
--     endpoint
--       "hi" (\ (_ :: (Int, Float, (), Int, Float, ())) -> pure ())
--   In an equation for ‘endpoint’:
--       endpoint
--         = endpoint
--             "hi" (\ (_ :: (Int, Float, (), Int, Float, ())) -> pure ())
