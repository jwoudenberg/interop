{-# LANGUAGE DeriveGeneric #-}

module TwoConstructorsWithoutRecordOneParam () where

import GHC.Generics (Generic)
import qualified Interop

data TwoConstructorsWithoutRecordOneParam
  = OneConstructor
  | TwoConstructor Int
  deriving (Generic)

instance Interop.Wire TwoConstructorsWithoutRecordOneParam

-- Compilation error:
--
-- • I can't create a Wire instance for this type:
--
--     data TwoConstructorsWithoutRecordOneParam
--       = ...
--       | TwoConstructor Int
--
--   I only support constructors with no parameters, or with a
--   a single parameter that must also be a record.
--   This is to make it easier for you to make changes to your
--   types in the future, in a backwards-compatible way.
--   Try creating a custom record type:
--
--     data TwoConstructorsWithoutRecordOneParam
--       = ...
--       | TwoConstructor TwoConstructorRecord
--
--     data TwoConstructorRecord = TwoConstructorRecord
--       { x :: Int
--       }
--
-- • In the expression: $dmrec @(TwoConstructorsWithoutRecordOneParam)
--   In an equation for ‘rec’:
--       rec = $dmrec @(TwoConstructorsWithoutRecordOneParam)
--   In the instance declaration for
--     ‘Wire TwoConstructorsWithoutRecordOneParam’
