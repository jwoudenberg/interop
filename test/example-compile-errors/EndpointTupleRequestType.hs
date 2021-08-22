{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EndpointTupleRequestType () where

import GHC.Generics (Generic)
import qualified Interop

endpoint :: Interop.Endpoint IO
endpoint = Interop.endpoint "hi" (\(_ :: (Int, Bool)) -> pure ())

-- Compilation error:
--
-- • You're using a tuple type in your endpoint:
--
--     (Int, Bool)
--
--   I don't support tuples as request or response types.
--
--   I prefer records over tuples, because those will allow you to make
--   backwards-compatible changes in the future.
--   Try using record syntax:
--
--     data MyRecord = MyRecord
--       { x :: Int
--       , y :: Bool
--       }
--
--   But come up with some better field names than MyRecord, x, and y!
--
-- • In the expression:
--     endpoint "hi" (\ (_ :: (Int, Bool)) -> pure ())
--   In an equation for ‘endpoint’:
--       endpoint = endpoint "hi" (\ (_ :: (Int, Bool)) -> pure ())
