{-# LANGUAGE DeriveGeneric #-}

module RecordWithNonWireField () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data RecordWithNonWireField = RecordWithNonWireField
  { fn :: NonGenericType
  }
  deriving (Generic)

data NonGenericType = NonGenericType

instance Wire.Wire RecordWithNonWireField

-- Compilation error:
--
-- • All the field types of a record with a Wire instance must themselves have a Wire instance.
-- • In the expression: $dmrec @(RecordWithNonWireField)
--   In an equation for ‘rec’: rec = $dmrec @(RecordWithNonWireField)
--   In the instance declaration for ‘Wire RecordWithNonWireField’
