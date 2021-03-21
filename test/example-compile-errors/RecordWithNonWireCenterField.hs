{-# LANGUAGE DeriveGeneric #-}

module RecordWithNonWireCenterField () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data RecordWithNonWireCenterField = RecordWithNonWireCenterField
  { int :: Int,
    fn :: NonWireType,
    tuple :: ()
  }
  deriving (Generic)

data NonWireType = NonWireType

instance Wire.Wire RecordWithNonWireCenterField

-- Compilation error:
--
-- • I'm trying to make a Wire instance of this type:
--
--     data RecordWithNonWireCenterField = RecordWithNonWireCenterField
--       { ...
--       , fn :: NonWireType
--       , ...
--       }
--
--   I need all the field types to have a Wire instance themselves,
--   but miss an instance for the type: NonWireType
--
-- • In the expression: $dmrec @(RecordWithNonWireCenterField)
--   In an equation for ‘rec’:
--       rec = $dmrec @(RecordWithNonWireCenterField)
--   In the instance declaration for ‘Wire RecordWithNonWireCenterField’
