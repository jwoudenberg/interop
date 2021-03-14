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
-- • No instance for (Wire NonGenericType)
--     arising from a use of ‘$dmrec’
-- • In the expression: $dmrec @(RecordWithNonWireField)
--   In an equation for ‘rec’: rec = $dmrec @(RecordWithNonWireField)
--   In the instance declaration for ‘Wire RecordWithNonWireField’
