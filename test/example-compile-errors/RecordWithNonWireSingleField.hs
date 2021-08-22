{-# LANGUAGE DeriveGeneric #-}

module RecordWithNonWireSingleField () where

import GHC.Generics (Generic)
import qualified Interop

data RecordWithNonWireSingleField = RecordWithNonWireSingleField
  { fn :: NonWireType
  }
  deriving (Generic)

data NonWireType = NonWireType

instance Interop.Wire RecordWithNonWireSingleField

-- Compilation error:
--
-- • I'm trying to make a Wire instance of this type:
--
--     data RecordWithNonWireSingleField = RecordWithNonWireSingleField
--       { fn :: NonWireType
--       }
--
--   I need all the field types to have a Wire instance themselves,
--   but miss an instance for the type: NonWireType
--
-- • In the expression: $dmrec @(RecordWithNonWireSingleField)
--   In an equation for ‘rec’:
--       rec = $dmrec @(RecordWithNonWireSingleField)
--   In the instance declaration for ‘Wire RecordWithNonWireSingleField’
