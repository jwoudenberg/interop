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
--   I don't support tuples in field types, because it's hard to change them
--   in the future in a backwards-compatible fashion.
--
-- • In the expression: $dmrec @(RecordWithTuple)
--   In an equation for ‘rec’: rec = $dmrec @(RecordWithTuple)
--   In the instance declaration for ‘Wire RecordWithTuple’
