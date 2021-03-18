{-# LANGUAGE DeriveGeneric #-}

module RecordWithTuple () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data RecordWithTuple = RecordWithTuple
  { fn :: (Int, ())
  }
  deriving (Generic)

instance Wire.Wire RecordWithTuple

-- Compilation error:
--
-- • I'm trying to make a Wire instance of this type:
--
--     data RecordWithTuple = RecordWithTuple
--       { fn :: (Int, ())
--       }
--
--   I need all the field types to have a Wire instance themselves,
--   but miss an instance for the type: (Int, ())
--
-- • In the expression: $dmrec @(RecordWithTuple)
--   In an equation for ‘rec’: rec = $dmrec @(RecordWithTuple)
--   In the instance declaration for ‘Wire RecordWithTuple’
