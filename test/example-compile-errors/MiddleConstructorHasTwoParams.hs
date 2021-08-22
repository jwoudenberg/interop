{-# LANGUAGE DeriveGeneric #-}

module MiddleConstructorHasTwoParams () where

import GHC.Generics (Generic)
import qualified Interop

data MiddleConstructorHasTwoParams
  = OneConstructor
  | TwoConstructor Int ()
  | ThreeConstructor
  deriving (Generic)

instance Interop.Wire MiddleConstructorHasTwoParams

-- Compilation error:
--
-- • I can't create a Wire instance for this type:
--
--     data MiddleConstructorHasTwoParams
--       = ...
--       | TwoConstructor Int ()
--       | ...
--
--   I only support constructors with no parameters, or with a
--   a single parameter that must also be a record.
--   This is to make it easier for you to make changes to your
--   types in the future, in a backwards-compatible way.
--   Try creating a custom record type:
--
--     data MiddleConstructorHasTwoParams
--       = ...
--       | TwoConstructor TwoConstructorRecord
--       | ...
--
--     data TwoConstructorRecord = TwoConstructorRecord
--       { x :: Int
--       , y :: ()
--       }
--
-- • In the expression: $dmrec @(MiddleConstructorHasTwoParams)
--   In an equation for ‘rec’:
--       rec = $dmrec @(MiddleConstructorHasTwoParams)
--   In the instance declaration for
--     ‘Wire MiddleConstructorHasTwoParams’
