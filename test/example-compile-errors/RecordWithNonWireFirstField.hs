{-# LANGUAGE DeriveGeneric #-}

module RecordWithNonWireFirstField () where

import GHC.Generics (Generic)
import qualified Interop

data RecordWithNonWireFirstField = RecordWithNonWireFirstField
  { fn :: NonWireType,
    int :: Int
  }
  deriving (Generic)

data NonWireType = NonWireType

instance Interop.Wire RecordWithNonWireFirstField

-- Compilation error:
--
-- • I'm trying to make a Wire instance of this type:
--
--     data RecordWithNonWireFirstField = RecordWithNonWireFirstField
--       { fn :: NonWireType
--       , ...
--       }
--
--   I need all the field types to have a Wire instance themselves,
--   but miss an instance for the type: NonWireType
--
-- • In the expression: $dmrec @(RecordWithNonWireFirstField)
--   In an equation for ‘rec’:
--       rec = $dmrec @(RecordWithNonWireFirstField)
--   In the instance declaration for ‘Wire RecordWithNonWireFirstField’
