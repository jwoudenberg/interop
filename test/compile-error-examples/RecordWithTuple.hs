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
-- • All the field types of a record with a Wire instance must themselves have a Wire instance.
-- • In the expression: $dmrec @(RecordWithTuple)
--   In an equation for ‘rec’: rec = $dmrec @(RecordWithTuple)
--   In the instance declaration for ‘Wire RecordWithTuple’
