{-# LANGUAGE DeriveGeneric #-}

module TwoConstructorsWithoutRecordTwoParams () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data ConstructorWithoutRecordTwoParams
  = OneConstructor
  | TwoConstructor Int ()
  | ThreeConstructor
  deriving (Generic)

instance Wire.Wire ConstructorWithoutRecordTwoParams

-- Compilation error:
--
-- • I can't create a Wire instance for this type:
--
--     data ConstructorWithoutRecordTwoParams
--       = ...
--       | TwoConstructor Int ()
--       | ...
--
--   I only support constructors with no parameters, or with a
--   a single parameter that must also be a record.
--   This is to make it easier for you to make changes to your
--   types in the future, in a  backwards-compatible way.
--   Try creating a custom record type:
--
--     data ConstructorWithoutRecordTwoParams
--       = ...
--       | TwoConstructor TwoConstructorRecord
--       | ...
--
--     data TwoConstructorRecord = TwoConstructorRecord
--       { x :: Int
--       , y :: ()
--       }
--
--   But come up with some better field names than x or y!
--
-- • In the expression: $dmrec @(ConstructorWithoutRecordTwoParams)
--   In an equation for ‘rec’:
--       rec = $dmrec @(ConstructorWithoutRecordTwoParams)
--   In the instance declaration for
--     ‘Wire ConstructorWithoutRecordTwoParams’
