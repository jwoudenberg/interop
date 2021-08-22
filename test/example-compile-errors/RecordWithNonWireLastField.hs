{-# LANGUAGE DeriveGeneric #-}

module RecordWithNonWireLastField () where

import GHC.Generics (Generic)
import qualified Interop

data RecordWithNonWireLastField = RecordWithNonWireLastField
  { int :: Int,
    fn :: NonWireType
  }
  deriving (Generic)

data NonWireType = NonWireType

instance Interop.Wire RecordWithNonWireLastField

-- Compilation error:
--
-- • I'm trying to make a Wire instance of this type:
--
--     data RecordWithNonWireLastField = RecordWithNonWireLastField
--       { ...
--       , fn :: NonWireType
--       }
--
--   I need all the field types to have a Wire instance themselves,
--   but miss an instance for the type: NonWireType
--
-- • In the expression: $dmrec @(RecordWithNonWireLastField)
--   In an equation for ‘rec’:
--       rec = $dmrec @(RecordWithNonWireLastField)
--   In the instance declaration for ‘Wire RecordWithNonWireLastField’
