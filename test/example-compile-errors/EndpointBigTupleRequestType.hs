{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EndpointBigTupleRequestType () where

import GHC.Generics (Generic)
import qualified Interop

endpoint :: Interop.Endpoint IO
endpoint = Interop.endpoint "hi" (\(_ :: (Int, Float, (), Int, Float, ())) -> pure ())

-- Compilation error:
--
-- • You're using a tuple type in your endpoint:
--
--     (Int, Float, (), Int, Float, ())
--
--   I don't support tuples as request or response types.
--
--   I prefer records over tuples, because those will allow you to make
--   backwards-compatible changes in the future.
--   Try using record syntax:
--
--     data MyRecord = MyRecord
--       { x :: Int
--       , y :: Float
--       , ...
--       }
--
--   But come up with some better field names than MyRecord, x, and y!
--
-- • In the expression:
--     endpoint
--       "hi" (\ (_ :: (Int, Float, (), Int, Float, ())) -> pure ())
--   In an equation for ‘endpoint’:
--       endpoint
--         = endpoint
--             "hi" (\ (_ :: (Int, Float, (), Int, Float, ())) -> pure ())
