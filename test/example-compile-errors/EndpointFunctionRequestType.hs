{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EndpointFunctionRequestType () where

import GHC.Generics (Generic)
import qualified Interop

endpoint :: Interop.Endpoint IO
endpoint = Interop.endpoint "hi" (\(_ :: Int -> Bool) -> pure ())

-- Compilation error:
--
-- • You're using a function type in your endpoint:
--
--     Int -> Bool
--
--   I don't support functions in endpoints types, because I don't know how
--   to encode functions to JSON.
--
-- • In the expression:
--     endpoint "hi" (\ (_ :: Int -> Bool) -> pure ())
--   In an equation for ‘endpoint’:
--       endpoint = endpoint "hi" (\ (_ :: Int -> Bool) -> pure ())
