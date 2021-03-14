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
-- • No instance for (Wire (Int, ())) arising from a use of ‘$dmrec’
-- • In the expression: $dmrec @(RecordWithTuple)
--   In an equation for ‘rec’: rec = $dmrec @(RecordWithTuple)
--   In the instance declaration for ‘Wire RecordWithTuple’
