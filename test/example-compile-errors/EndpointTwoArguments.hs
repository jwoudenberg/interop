{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EndpointTwoArguments () where

import GHC.Generics (Generic)
import qualified Interop

endpoint :: Interop.Endpoint IO
endpoint = Interop.endpoint "hi" (\(_ :: Int) (_ :: Bool) -> pure ())

-- Compilation error:
--
-- • Couldn't match expected type ‘IO res0’
--               with actual type ‘Bool -> f0 ()’
-- • The lambda expression ‘\ (_ :: Int) (_ :: Bool) -> pure ()’
--   has two arguments,
--   but its type ‘Int -> IO res0’ has only one
--   In the second argument of ‘endpoint’, namely
--     ‘(\ (_ :: Int) (_ :: Bool) -> pure ())’
--   In the expression:
--     endpoint "hi" (\ (_ :: Int) (_ :: Bool) -> pure ())
