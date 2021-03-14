{-# LANGUAGE DeriveGeneric #-}

module RecordWithFunction () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data RecordWithFunction = RecordWithFunction
  { fn :: Int -> ()
  }
  deriving (Generic)

instance Wire.Wire RecordWithFunction

-- Compilation error:
--
-- • All the field types of a record with a Wire instance must themselves have a Wire instance.
-- • In the expression: $dmrec @(RecordWithFunction)
--   In an equation for ‘rec’: rec = $dmrec @(RecordWithFunction)
--   In the instance declaration for ‘Wire RecordWithFunction’
